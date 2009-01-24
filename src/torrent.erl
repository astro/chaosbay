-module(torrent).

-export([init/0, add/2, add_http/1, add_from_dir/1,
	 recent/0, get_torrent_by_name/1,
	 add_comment/2, get_comments/1,
	 tracker_request/8]).

-include("../include/torrent.hrl").

-define(INFO_DOC(Name), "info:" ++ Name).
-define(TORRENT_DOC(Name), "torrent:" ++ Name).
-define(TRACKER_DOC64(Id), "tracker:" ++ Id).
-define(TRACKER_DOC(Id), ?TRACKER_DOC64(base64:encode(Id))).
-define(COMMENTS_DOC(Name), "comments:" ++ Name).


init() ->
    couch_lier:create_database(chaosbay, "localhost", 5984),
    ViewsDir =
	chaosbay_deps:local_path(["priv", "couchdb_views"]),
    couch_view:install_from_dir(chaosbay, ViewsDir).

add_http(URL) ->
    [Filename | _] = lists:reverse(string:tokens(URL, "/")),
    {ok, {{_, 200, _}, _Attrs, Body}} = http:request(URL),
    add(Filename, list_to_binary(Body)).
add_from_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    lists:map(fun(File) ->
		      %% TODO: use strip_suffixes/2?
			  FileLen = string:len(File),
			  case string:rstr(File, ".torrent") of
			      N when FileLen > 8, N == FileLen - 7 ->
				  Name = string:sub_string(File, 1, FileLen - 8),
				  io:format("Reading ~p~n",[Dir ++ "/" ++ File]),
				  {ok, Bin} = file:read_file(Dir ++ "/" ++ File),
				  R = add(Name, Bin),
				  {File, R};
			      _ -> {File, ignored}
			  end
		  end, Files).

add(Filename, Upload) when is_binary(Filename) ->
    add(binary_to_list(Filename), Upload);

add(Filename, Upload) ->
    NewFilename = strip_suffixes(Filename, ".torrent"),
    ParsedFile = benc:parse(Upload),
    HashId = torrent_info:info_hash(ParsedFile),
    Length = torrent_info:get_length(ParsedFile),
    ParsedFile2 = torrent_info:set_tracker(ParsedFile),
    Binary = benc:to_binary(ParsedFile2),
    Torrent = #torrent{name = NewFilename,
		       hash_id = HashId,
		       length = Length,
		       date = util:mk_timestamp(),
		       binary = Binary},
    {JSON_Info, JSON_Torrent} = J = torrent_to_json(Torrent),
    io:format("J: ~p~n",[J]),
    F = fun() ->
		case {couch_lier:read(chaosbay, ?INFO_DOC(NewFilename)),
		      couch_lier:read(chaosbay, ?TORRENT_DOC(NewFilename))} of
		    {{struct, []},
		     {struct, []}} ->
			couch_lier:write(chaosbay, ?INFO_DOC(NewFilename), JSON_Info),
			couch_lier:write(chaosbay, ?TORRENT_DOC(NewFilename), JSON_Torrent),
			couch_lier:write(chaosbay, ?TRACKER_DOC(HashId),
					 {struct, [{<<"torrent">>, list_to_binary(NewFilename)},
						   {<<"completed">>, 0},
						   {<<"peers">>, {struct, []}}
						  ]}),
			{ok, NewFilename};
		    _ ->
			exists
		end
	end,
    {atomic, Result} = couch_lier:transaction(F),
    Result.

strip_suffixes(S, Suffix) ->
    case lists:split(length(S) - length(Suffix), S) of
	{S2, Suffix} ->
	    strip_suffixes(S2, Suffix);
	_ ->
	    S
    end.



recent() ->
    {struct, DocDict} = couch_lier:dirty_read(chaosbay, <<"_view/info/recent">>),
    {value, {_, [{struct, RowsDict}]}} = lists:keysearch(<<"rows">>, 1, DocDict),
    {value, {_, Torrents}} = lists:keysearch(<<"value">>, 1, RowsDict),
    [json_to_torrent(Torrent)
     || Torrent <- Torrents].


get_torrent_by_name(Name) when is_binary(Name) ->
    get_torrent_by_name(binary_to_list(Name));

get_torrent_by_name(Name) ->
    F = fun() ->
		case {couch_lier:read(chaosbay, ?INFO_DOC(Name)),
		      couch_lier:read(chaosbay, ?TORRENT_DOC(Name))} of
		    {{struct, []},
		     {struct, []}} -> not_found;
		    {{struct, InfoDict} = JSON_Info, JSON_Torrent} ->
			{value, {_, HashId64}} =
			    lists:keysearch(<<"hash_id">>, 1, InfoDict),
			JSON_Tracker =
			    couch_lier:read(chaosbay, ?TRACKER_DOC64(HashId64)),
			{JSON_Info, JSON_Tracker, JSON_Torrent}			
		end
	end,
    case couch_lier:transaction(F) of
	{atomic, not_found} -> not_found;
	{atomic, {JSON_Info,
		  {struct, Dict_Tracker},
		  {struct, Dict_Torrent}}} ->
	    {value, {_, Completed}} =
		lists:keysearch(<<"completed">>, 1, Dict_Tracker),
	    {value, {_, {struct, Peers}}} =
		lists:keysearch(<<"peers">>, 1, Dict_Tracker),
	    {Seeders, Leechers} =
		lists:foldl(fun(Peer, {S, L}) ->
				    case json_to_peer(Peer) of
					{_, seeder, _, _} ->
					    {S + 1, L};
					{_, leecher, _, _} ->
					    {S, L + 1}
				    end
			    end, {0, 0}, Peers),
	    Torrent = json_to_torrent(JSON_Info),
	    {value, {_, Binary}} =
		lists:keysearch(<<"data">>, 1, Dict_Torrent),
	    Torrent#torrent{binary = base64:decode(Binary),
			    seeders = Seeders,
			    leechers = Leechers,
			    completed = Completed}
    end.


torrent_to_json(#torrent{} = Torrent) ->
    JSON_Info =
	{struct, [{name, list_to_binary(Torrent#torrent.name)},
		  {hash_id, base64:encode(Torrent#torrent.hash_id)},
		  {length, Torrent#torrent.length},
		  {category, list_to_binary(Torrent#torrent.category)},
		  {date, Torrent#torrent.date}
		 ]},
    JSON_Torrent =
	{struct, [{data, base64:encode(Torrent#torrent.binary)}]},
    {JSON_Info, JSON_Torrent}.

json_to_torrent(JSON) ->
    #torrent{name = get_json_dict_val(name, JSON),
	     hash_id = base64:decode(get_json_dict_val(hash_id, JSON)),
	     length = get_json_dict_val(length, JSON),
	     category = get_json_dict_val(category, JSON),
	     date = get_json_dict_val(date, JSON),
	     comments = get_json_dict_val(comments, JSON, unknown),
	     seeders = get_json_dict_val(seeders, JSON, unknown),
	     leechers = get_json_dict_val(leechers, JSON, unknown),
	     completed = get_json_dict_val(completed, JSON, unknown)}.


get_json_dict_val(Key, JSON) when is_atom(Key) ->
    get_json_dict_val(atom_to_list(Key), JSON);
get_json_dict_val(Key, JSON) when is_list(Key) ->
    get_json_dict_val(list_to_binary(Key), JSON);
get_json_dict_val(Key, {struct, Dict}) when is_binary(Key) ->
    {value, {_, Value}} = lists:keysearch(Key, 1, Dict),
    Value.

get_json_dict_val(Key, JSON, Default) when is_atom(Key) ->
    get_json_dict_val(atom_to_list(Key), JSON, Default);
get_json_dict_val(Key, JSON, Default) when is_list(Key) ->
    get_json_dict_val(list_to_binary(Key), JSON, Default);
get_json_dict_val(Key, {struct, Dict}, Default) when is_binary(Key) ->
    case lists:keysearch(Key, 1, Dict) of
	{value, {_, Value}} ->
	    Value;
	false ->
	    Default
    end.


add_comment(Name, Text) when is_binary(Name) ->
    add_comment(binary_to_list(Name), Text);

add_comment(Name, Text) when is_list(Text) ->
    add_comment(Name, list_to_binary(Text));

add_comment(Name, Text) ->
    Now = util:mk_timestamp(),
    F = fun() ->
		{struct, OldDict} =
		    case couch_lier:read(chaosbay, ?COMMENTS_DOC(Name)) of
			{struct, []} ->
			    {struct, [{<<"comments">>, []}]};
			{struct, _Dict} = J ->
			    J
		    end,
		OldComments = case lists:keysearch(<<"comments">>, 1, OldDict) of
				  {value, {_, OldComments_}} ->
				      OldComments_;
				  false ->
				      []
			      end,
		NewComments = OldComments ++
		    [{struct, [{<<"date">>, Now},
			       {<<"text">>, Text}]}],
		NewDict = lists:keystore(<<"comments">>, 1, OldDict,
					 {<<"comments">>, NewComments}),
		couch_lier:write(chaosbay, ?COMMENTS_DOC(Name), {struct, NewDict})
	end,
    {atomic, _} = couch_lier:transaction(F).

get_comments(Name) when is_binary(Name) ->
    get_comments(binary_to_list(Name));

get_comments(Name) ->
    F = fun() ->
		couch_lier:read(chaosbay, ?COMMENTS_DOC(Name))
	end,
    {atomic, {struct, Dict}} = couch_lier:transaction(F),
    case lists:keysearch(<<"comments">>, 1, Dict) of
	{value, {_, Comments}} ->
	    lists:map(fun json_to_comment/1, Comments);
	false ->
	    []
    end.

json_to_comment(JSON) ->
    #comment{date = get_json_dict_val(date, JSON),
	     text = get_json_dict_val(text, JSON)}.


tracker_request(HashId, PeerId, IP, Port, Uploaded, Downloaded, Left, Event) ->
    PeerKey = base64:encode(PeerId),
    Now = util:mk_timestamp(),
    F = fun() ->
		case couch_lier:read(chaosbay, ?TRACKER_DOC(HashId)) of
		    {struct, []} ->
			not_found;
		    {struct, Dict1} ->
			{Action, Dict3} =
			    case Event of
				<<"completed">> ->
				    {value, {_, Completed}} =
					lists:keysearch(<<"completed">>, 1, Dict1),
				    Dict2 = lists:keystore(<<"completed">>, 1, Dict1,
							   {<<"completed">>, Completed + 1}),
				    {add, Dict2};
				<<"stopped">> ->
				    {remove, Dict1};
				_ ->
				    {add, Dict1}
			    end,
			{value, {_, {struct, Peers1}}} =
			    lists:keysearch(<<"peers">>, 1, Dict3),
			Peers2 =
			    case Action of
				add ->
				    lists:keystore(PeerKey, 1, Peers1,
						   {PeerKey,
						    {struct, [{<<"ip">>, list_to_binary(IP)},
							      {<<"port">>, Port},
							      {<<"uploaded">>, Uploaded},
							      {<<"downloaded">>, Downloaded},
							      {<<"left">>, Left},
							      {<<"seen">>, Now}]}});
				remove ->
				    lists:keydelete(PeerKey, 1, Peers1)
			    end,
			Dict4 =
			    lists:keystore(<<"peers">>, 1, Dict3,
					   {<<"peers">>, {struct, Peers2}}),
			couch_lier:write(chaosbay, ?TRACKER_DOC(HashId),
					 {struct, Dict4}),
			Peers2
		end
	end,
    case couch_lier:transaction(F) of
	{atomic, not_found} ->
	    not_found;
	{atomic, PeersDict} ->
	    Peers = [json_to_peer(PeerJSON)
		     || PeerJSON <- PeersDict],
	    PeersWithoutMe = lists:filter(
			       fun({PeerId1, _Type, _IP1, _Port1}) ->
				       PeerId1 =/= PeerId
			       end, Peers),
	    %% return only leechers for seeders
	    PeersInteresting = case Left of
				   0 -> lists:filter(
					  fun({_, Type, _, _}) ->
						  Type =:= leecher
					  end,
					  PeersWithoutMe);
				   _ -> PeersWithoutMe
			       end,
	    PeersWithoutType = [{PeerId1, IP1, Port1}
				|| {PeerId1, _Type, IP1, Port1} <- PeersInteresting],
	    {peers, pick_randomly(PeersWithoutType, 10)}
    end.

json_to_peer({PeerId, JSON}) ->
    Left = get_json_dict_val(left, JSON),
    Type = case Left of
	       0 -> seeder;
	       _ -> leecher
	   end,
    {base64:decode(PeerId),
     Type,
     get_json_dict_val(ip, JSON),
     get_json_dict_val(port, JSON)}.

pick_randomly(_, 0) -> [];
pick_randomly([], _) -> [];
pick_randomly(List, NToPick) ->
    E = lists:nth(random:uniform(length(List)), List),
    List2 = lists:delete(E, List),
    [E | pick_randomly(List2, NToPick - 1)].
