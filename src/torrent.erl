-module(torrent).

-export([init/0,
	 add/2, add_http/1, add_from_dir/1,
	 reset_tracker_urls/0,
	 recent/1,
	 get_torrent_meta_by_name/1, torrent_name_by_id_t/1,
	 get_torrent_binary/1]).

-include("../include/torrent.hrl").


init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    util:safe_mnesia_create_table(torrent_meta, [{disc_copies, [node()]},
				  {attributes, record_info(fields, torrent_meta)}]),
    mnesia:add_table_index(torrent, id),
    util:safe_mnesia_create_table(torrent_data, [{disc_only_copies, [node()]},
				  {attributes, record_info(fields, torrent_data)}]).


add_http(URL) ->
    [Filename | _] = lists:reverse(string:tokens(URL, "/")),
    {ok, {{_, 200, _}, _Attrs, Body}} = http:request(URL),
    add(Filename, list_to_binary(Body)).
add_from_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    lists:map(fun(File) ->
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

add(Filename, _Upload) when Filename == ""; Filename == <<>> ->
    invalid;

add(Filename, Upload) when is_list(Filename) ->
    add(list_to_binary(Filename), Upload);

add(Filename, Upload) ->
    NewFilename = case split_binary(Filename, size(Filename) - 8) of
		      {FN, <<".torrent">>} -> FN;
		      _ -> Filename
		  end,
    case (catch benc:parse(Upload)) of
	{'EXIT', _Reason} -> {error, "Cannot parse this file"};
	ParsedFile ->
	    %%Body = io_lib:format("<pre>file: ~s (~s)~n~p</pre>",[FileName, FileType, ParsedFile]),
	    Id = torrent_info:info_hash(ParsedFile),
	    Length = torrent_info:get_length(ParsedFile),
	    ParsedFile2 = torrent_info:set_tracker(ParsedFile),
	    Binary = benc:to_binary(ParsedFile2),
	    TorrentMeta = #torrent_meta{name = NewFilename,
			       id = Id,
			       length = Length,
			       date = util:mk_timestamp()},
	    TorrentData = #torrent_data{name = NewFilename,
					binary = Binary},
	    F = fun() ->
			mnesia:write_lock_table(torrent),
			case mnesia:read({torrent_meta, NewFilename}) of
			    [] ->
				mnesia:write(TorrentMeta),
				mnesia:write(TorrentData),
				{ok, NewFilename};
			    _ ->
				exists
			end
		end,
	    {atomic, Result} = mnesia:transaction(F),
	    Result
    end.


reset_tracker_urls() ->
    F = fun() ->
		mnesia:write_lock_table(torrent_data),
		mnesia:foldl(fun(#torrent_data{binary = Binary} = TorrentData, _) ->
				     Parsed = benc:parse(Binary),
				     Parsed2 = torrent_info:set_tracker(Parsed),
				     Binary2 = benc:to_binary(Parsed2),
				     mnesia:write(TorrentData#torrent_data{binary = Binary2})
			     end, 0, torrent_data)
	end,
    {atomic, _} = mnesia:transaction(F).


recent(Max) ->
    F = fun() ->
		S =
		    mnesia:foldl(fun(TorrentMeta, Result) ->
					 sorted:insert(Result, TorrentMeta)
				 end,
				 sorted:new(#torrent_meta.date, desc, Max),
				 torrent_meta),
		sorted:to_list(S)
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.


get_torrent_meta_by_name(Name) when is_list(Name) ->
    get_torrent_meta_by_name(list_to_binary(Name));

get_torrent_meta_by_name(Name) ->
    F = fun() ->
		case mnesia:read({torrent_meta, Name}) of
		    [TorrentMeta] -> TorrentMeta;
		    [] -> not_found
		end
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.


torrent_name_by_id_t(Id) ->
    case mnesia:index_read(torrent_meta, Id, #torrent_meta.id) of
	[] -> not_found;
	[#torrent_meta{name = Name}] -> {ok, Name}
    end.

get_torrent_binary(Name) when is_list(Name) ->
    get_torrent_binary(list_to_binary(Name));
get_torrent_binary(Name) ->
    case mnesia:dirty_read(torrent_data, Name) of
	[#torrent_data{binary = Binary}] ->
	    {ok, Binary};
	_ ->
	    not_found
    end.
