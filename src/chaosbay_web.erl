%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for chaosbay.

-module(chaosbay_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/1]).

-include("../include/torrent.hrl").
-include("../include/comment.hrl").

-define(TRACKER_REQUEST_INTERVAL, 60).
-define(RESULTSET_LENGTH, 50).

-define(MIME_XHTML, "application/xhtml+xml").
-define(MIME_ATOM, "application/atom+xml").
-define(MIME_BITTORRENT, "application/x-bittorrent").

%% External API

start(Options) ->
    Loop = fun (Req) ->
                   ?MODULE:loop(Req)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).


loop(Req) ->
    Path = Req:get(path),
    Method = case Req:get(method) of
		 M when M =:= 'GET'; M =:= 'HEAD' -> 'GET';
		 M -> M
	     end,
    Path2 = lists:dropwhile(fun(C) -> C == $/ end,
			    Path),
    T1 = util:mk_timestamp_us(),
    Response = case (catch request(Req, Method, Path2)) of
		   {'EXIT', Reason} ->
		       io:format("Error for ~p ~p:~n\t~p~n~n", [Method, Path2, Reason]),
		       %% TODO: return like 500 here
		       ReasonS = lists:flatten(io_lib:format("~p", [Reason])),
		       HTML =
			   [{h2, ["Invalid stuff happened"]},
			    {p, [{"class", "important error code"}], [ReasonS]}],
		       Body = lists:map(fun html:to_iolist/1, HTML),
		       Req:respond({500, [{"Content-type", ?MIME_XHTML}], html_skeleton(Body)});
		   Response1 -> Response1
	       end,
    T2 = util:mk_timestamp_us(),
    io:format("~s [~Bus] ~s ~s~n", [Req:get(peer), T2 - T1, Method, Path]),
    collectd:set_gauge(delay, http_request, [(T2 - T1) / 1000000]),
    Response.


%% Internal API

get_remote_addr_(Addr) ->
    case inet:getaddr(Addr, inet6) of
	{ok, {0, 0, 0, 0, 0, 16#ffff, AB, CD}} ->
	    {AB bsr 8, AB band 16#ff, CD bsr 8, CD band 16#ff};
	{ok, Addr6} ->
	    Addr6
    end.
-define(GET_REMOTE_ADDR, get_remote_addr_(Req:get(peer))).

count_request_(What, {_, _, _, _}) ->
    collectd:inc_counter(http_requests, "inet_" ++ atom_to_list(What), [1]);
count_request_(What, {_, _, _, _, _, _, _, _}) ->
    collectd:inc_counter(http_requests, "inet6_" ++ atom_to_list(What), [1]).
-define(COUNT_REQUEST(What), count_request_(What, ?GET_REMOTE_ADDR)).

-define(COL_LINK(Field), (case atom_to_list(Field) of
			      SortName ->
				  browse_link(SortName, OtherDirection, 0, Pattern);
			      FieldS ->
				  browse_link(FieldS, column_default_direction(Field), 0, Pattern)
			  end)).

column_default_direction(speed) -> desc;
column_default_direction(seeders) -> desc;
column_default_direction(leechers) -> desc;
column_default_direction(comments) -> desc;
column_default_direction(_) -> asc.

request(Req, 'GET', "add") ->
    ?COUNT_REQUEST(add_form),
    Body = [<<"
<h2>Add a .torrent file</h2>
<form action='/add' method='POST' enctype='multipart/form-data' class='important'>
  <input type='file' name='file' accept='">>, ?MIME_BITTORRENT, <<"' maxlength='524288'/>
  <input type='submit' value='Add'/>
</form>
<h3>Tracker information will be set automatically!</h3>
<p class='note'>
I <b>replace</b> the tracker URL in your file with our own. That enables me
to get numbers of Seeders &amp; Leechers.
</p>
<h3>Please leech the files first.</h3>
<p class='note'>
It's kinda stupid without any seeders.
</p>
<h3>How do I create a .torrent file?</h3>
<p>
  If your favourite client can't there's always the
  <a href='http://www.bittorrent.com/'>original BitTorrent client</a>,
  <a href='http://claudiusmaximus.goto10.org/index.php?page=coding/buildtorrent'>buildtorrent</a>
  or <a href='http://btfaq.com/serve/cache/14.html'>many others.</a>
  Examples:
</p>
<pre>buildtorrent -a ">>, chaosbay:absolute_path("/announce"), <<" <i>SourceDirectory</i> <i>TargetFile.torrent</i>
createtorrent -a ">>, chaosbay:absolute_path("/announce"), <<" <i>SourceDirectory</i> <i>TargetFile.torrent</i>
</pre>
<h3>Script me!</h3>
<p>
  Uploading a batch of .torrent files with the shell you trust? No problem for <a href='http://curl.haxx.se/'>curl</a>:
</p>
<pre>curl -X POST -F file=@<i>foobar.torrent</i> ">>, chaosbay:absolute_path("/add"), <<"</pre>
">>],
    html_ok(Req, Body);

request(Req, 'POST', "add") ->
    ?COUNT_REQUEST(add),
    Multipart = mochiweb_multipart:parse_form(Req),
    {FileName, {_FileType, _}, File} = proplists:get_value("file", Multipart),
    case torrent:add(FileName, File) of
	{ok, Name} ->
	    html_ok(Req, [<<"
<h2>Very good!</h2>
<p>Now please download our .torrent file and seed that!</p>
<p class='important'>Download <a href='">>, link_to_torrent(Name), <<"'>">>, Name, <<"</a></p>
<p>Then, view <a href='">>, link_to_details(Name), <<"'>details</a>, go back to the <a href='/'>index</a> or <a href='/add'>add</a> another Torrent.</p>
">>]);
	exists ->
	    html_ok(Req, <<"
<h2>Sorry</h2>
<p class='important'>I already have this file.</p>
<p>You might want to rename it if the contents are really different.</p>
">>);
	invalid ->
	    request(Req, 'GET', "add");
	{error, ReasonS} ->
	    HTML =
		[{h2, ["Invalid stuff happened"]},
		 {p, [{"class", "important error code"}], [ReasonS]}],
	    Body = lists:map(fun html:to_iolist/1, HTML),
	    Req:respond({406, [{"Content-type", ?MIME_XHTML}], html_skeleton(Body)})
    end;

request(Req, 'GET', "") ->
    ?COUNT_REQUEST(root),
    Req:respond({301, [{"Location", browse_link(age, asc, 0, "")}], []});

request(Req, 'GET', "search") ->
    QS = Req:parse_qs(),
    {value, {_, QueryEncoded}} = lists:keysearch("q", 1, QS),
    Pattern = mochiweb_util:unquote(QueryEncoded),
    Req:respond({301, [{"Location", browse_link(seeders, desc, 0, Pattern)}], []});

request(Req, 'GET', "browse/" ++ Path) ->
    ?COUNT_REQUEST(browse),
    [SortName, DirectionS, OffsetS, PatternEncoded] = util:split_string(Path, $/, 4),
    {Direction, OtherDirection} = case DirectionS of
				      "a" -> {asc, desc};
				      "d" -> {desc, asc}
				  end,
    Offset = list_to_integer(OffsetS),
    Pattern = mochiweb_util:unquote(PatternEncoded),
    {TorrentMetas, TorrentTotal} = torrent_browse:search(Pattern,
							 ?RESULTSET_LENGTH, Offset,
							 SortName, Direction),
    HTML =
	[{form, [{"id", "search"},
		 {"method", "get"},
		 {"action", "/search"}],
	  [{input, [{"type", "text"},
		    {"name", "q"},
		    {"id", "q"},
		    {"title", "All space-seperated words will AND-match. Glob patterns (*?) allowed."},
		    {"length", "40"},
		    {"value", Pattern}], []},
	   {input, [{"type", "submit"},
		    {"value", "Search"}], []}
	  ]},
	 {table, [{"border", "1"}],
	  [{tr, [{th, [{a, [{"href", ?COL_LINK(name)}], ["Name"]}]},
		 {th, [""]},
		 {th, [{a, [{"href", ?COL_LINK(size)}], ["Size"]}]},
		 {th, [{a, [{"href", ?COL_LINK(age)}], ["Age"]}]},
		 {th, [{"title", "Comments"}],
		  [{a, [{"href", ?COL_LINK(comments)}], ["C"]}]},
		 {th, [{"title", "Seeders"}],
		  [{a, [{"href", ?COL_LINK(seeders)}], ["S"]}]},
		 {th, [{"title", "Leechers"}],
		  [{a, [{"href", ?COL_LINK(seechers)}], ["L"]}]},
		 {th, [{a, [{"href", ?COL_LINK(speed)}], ["Speed"]}]}
		]}
	   | lists:map(fun(#browse_result{name = Name,
					  length = Length,
					  age = Age, comments = C,
					  seeders = S, leechers = L,
					  speed = Speed}) ->
			       Class = case {S, L} of
					   {0, 0} -> "dead";
					   {0, _} -> "starving";
					   {_, _} -> ""
				       end,
			       LinkDetails = link_to_details(Name),
			       LinkTorrent = link_to_torrent(Name),
			       {tr, [{"class", Class}],
				[{td, [{a, [{"href", LinkDetails}],
					[Name]}]},
				 {td, [{a, [{"href", LinkTorrent},
					    {"class", "download"}],
					["Get"]}]},
				 {td, [util:human_length(Length)]},
				 {td, [util:human_duration(Age)]},
				 {td, [{"style", if
						     C == 0 -> "color: #aaa";
						     true -> "font-weight:bold"
						 end}],
				  [integer_to_list(C)]},
				 {td, [integer_to_list(S)]},
				 {td, [integer_to_list(L)]},
				 {td, [util:human_bandwidth(Speed)]}
				]}
		       end, TorrentMetas)]},
	 {p, [{"id", "pages"}],
	  [{a, if PageOffset == Offset -> [{"id", "current"}];
		  true -> [{"href", browse_link(SortName, Direction, PageOffset, Pattern)}]
	       end,
	    [lists:flatten(io_lib:format("~B", [PageNumber]))]}
	   || {PageNumber, PageOffset} <- generate_pages(TorrentTotal)]}
	],
    Body = lists:map(fun html:to_iolist/1, HTML),
    html_ok(Req, Body);

request(Req, 'GET', "atom") ->
    ?COUNT_REQUEST(atom),
    TorrentMetas = torrent:recent(50),
    Atom = {feed, [{"xmlns", "http://www.w3.org/2005/Atom"}],
	    [{title, [<<"Chaos Bay">>]},
	     {id, [chaosbay:absolute_path("/")]},
	     {link, [{"rel", "self"},
		     {"type", ?MIME_ATOM},
		     {"href", chaosbay:absolute_path("/atom")}], []},
	     {link, [{"rel", "alternate"},
		     {"type", ?MIME_XHTML},
		     {"href", chaosbay:absolute_path("/")}], []}
	     | lists:map(fun(#torrent_meta{name = Name,
					   id = Id,
					   date = Date,
					   length = Length}) ->
				 {ok, Binary} = torrent:get_torrent_binary(Name),
				 {S, L, Speed} = tracker:tracker_info(Id),
				 LinkDetails = chaosbay:absolute_path(link_to_details(Name)),
				 LinkTorrent = chaosbay:absolute_path(link_to_torrent(Name)),
				 Date8601 = util:timestamp_to_iso8601(Date),
				 {entry, [
					  {title, [Name]},
					  {id, [LinkTorrent]},
					  {published, [Date8601]},  %% TODO: last tracker activity
					  {updated, [Date8601]},
					  {link, [{"rel", "alternate"},
						  {"type", ?MIME_XHTML},
						  {"href", [LinkDetails]}], []},
					  {link, [{"rel", "enclosure"},
						  {"type", ?MIME_BITTORRENT},
						  {"length", size(Binary)},
						  {"href", LinkTorrent}], []},
					  {summary, [{"type", "xhtml"}],
					   [{'div', [{"xmlns", "http://www.w3.org/1999/xhtml"}],
					     [{dl,
					       [{dt, [<<"Download">>]},
						{dd, [{a, [{"href", LinkTorrent}],
						       [list_to_binary([Name, <<".torrent">>])]}]},
						{dt, [<<"Size">>]},
						{dd, [util:human_length(Length)]},
						{dt, [<<"Seeders">>]},
						{dd, [integer_to_list(S)]},
						{dt, [<<"Leechers">>]},
						{dd, [integer_to_list(L)]},
						{dt, [<<"Speed">>]},
						{dd, [util:human_bandwidth(Speed)]}]
					      }]}
					   ]}
					 ]}
			 end, TorrentMetas)]},
    Body = html:to_iolist(Atom),
    Req:ok({?MIME_ATOM,
	    [<<"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n\n">>, Body]});
		   
request(Req, 'GET', "static/" ++ Path) ->
    ?COUNT_REQUEST(static),
    DocRoot = chaosbay_deps:local_path(["priv", "www"]),
    Req:serve_file(Path, DocRoot, [{"Cache-Control", "max-age=7200"},
				   {"Expires", "Thu, 30 Oct 2008 23:42:59 GMT"}]);

request(Req, Method, "comments/" ++ Name)
  when Method =:= 'GET';
       Method =:= 'POST' ->

    if
	Method =:= 'POST' ->
	    ?COUNT_REQUEST(post_comment),
	    Posted = Req:parse_post(),
	    {value, {_, Text}} = lists:keysearch("text", 1, Posted),
	    comment:add(Name, Text);
	true ->
	    ?COUNT_REQUEST(get_comments),
	    no_post
    end,

    Body = lists:map(
	     fun(#comment{date = Date,
			  text = Text}) ->
		     HTML =
			 [{h3,
			   [list_to_binary([util:human_duration(
					      util:mk_timestamp() - Date),
					    " ago"])]},
			  {pre, [Text]}],
		     lists:map(fun html:to_iolist/1, HTML)
	     end, comment:get_comments(Name)),
    Req:ok({"text/html",
	    [Body, <<"
<h3>Post</h3>
<form onsubmit='submitComment(); return false;'>
  <textarea id='comment-text' cols='40' rows='5'></textarea>
  <br/>
  <input type='submit' value='post'/>
</form>
">>]});

request(Req, 'GET', "announce") ->
    ?COUNT_REQUEST(announce),
    
    IP = ?GET_REMOTE_ADDR,
    QS = Req:parse_qs(),
    {value, {_, InfoHash1}} = lists:keysearch("info_hash", 1, QS),
    InfoHash = list_to_binary(InfoHash1),
    {value, {_, PeerId1}} = lists:keysearch("peer_id", 1, QS),
    PeerId = list_to_binary(PeerId1),
    Reply =
	case lists:keysearch("event", 1, QS) of
	    {value, {_, "stopped"}} ->
		tracker:tracker_request_stopped(InfoHash, PeerId, IP),
		[{<<"ok">>, <<"true">>}];
	    
	    _ ->
		{value, {_, Port1}} = lists:keysearch("port", 1, QS),
		{Port, []} = string:to_integer(Port1),
		Uploaded = case lists:keysearch("uploaded", 1, QS) of
			       {value, {_, Uploaded1}} ->
				   case string:to_integer(Uploaded1) of
				       {Uploaded2, _} when is_integer(Uploaded2) ->
					   Uploaded2;
				       _ -> 0
				   end;
			       false -> 0
			   end,
		Downloaded = case lists:keysearch("downloaded", 1, QS) of
				 {value, {_, Downloaded1}} -> 
				     case string:to_integer(Downloaded1) of
					 {Downloaded2, _} when is_integer(Downloaded2) ->
					     Downloaded2;
					 _ -> 0
				     end;
				 false -> 0
			     end,
		{value, {_, Left1}} = lists:keysearch("left", 1, QS),
		{Left, _} = string:to_integer(Left1),
		Result =
		    tracker:tracker_request(InfoHash, PeerId,
					    IP, Port,
					    Uploaded, Downloaded, Left),
		case Result of
		    not_found ->
			[{<<"failure reason">>, <<"No torrent registered for info_hash">>}];
		    {peers, Peers} ->
			case lists:keysearch("compact", 1, QS) of
			    {value, _} ->
				build_compact_tracker_response(Peers);
			    _ ->
				build_tracker_response(Peers)
			end
		end
	end,
    Bencoded = benc:to_binary(Reply),
    Req:ok({?MIME_BITTORRENT,
	    Bencoded});

request(Req, 'GET', "scrape") ->
    ?COUNT_REQUEST(scrape),
    
    QS = Req:parse_qs(),
    {value, {_, InfoHash1}} = lists:keysearch("info_hash", 1, QS),
    InfoHash = list_to_binary(InfoHash1),
    Reply = case torrent:get_torrent_meta_by_id(InfoHash) of
		{ok, #torrent_meta{name = Name,
				   length = Length}} ->
		    {Complete, Incomplete, Downloaded} = tracker:tracker_scrape(InfoHash),
		    DownloadCount = trunc(Downloaded / Length),
		    [{<<"files">>,
		      [{InfoHash,
			[{<<"complete">>, Complete},
			 {<<"incomplete">>, Incomplete},
			 {<<"downloaded">>, DownloadCount},
			 {<<"name">>, Name}]}]},
		     {<<"flags">>,
		      [{<<"min_request_interval">>, ?TRACKER_REQUEST_INTERVAL}]}
		    ];
		not_found ->
		    [{<<"failure reason">>, <<"No torrent registered for info_hash">>},
		     {<<"flags">>,
		      [{<<"min_request_interval">>, ?TRACKER_REQUEST_INTERVAL}]}
		    ]
	   end,
    Bencoded = benc:to_binary(Reply),
    Req:ok({?MIME_BITTORRENT,
	    Bencoded});

request(Req, 'GET', {download, Name}) ->
    case torrent:get_torrent_binary(Name) of
	{ok, Binary} ->
	    ?COUNT_REQUEST(download),
	    Req:ok({?MIME_BITTORRENT,
		    Binary});
	not_found ->
	    ?COUNT_REQUEST(download404),
	    html_not_found(Req)
    end;

request(Req, 'GET', {details, Name}) ->
    case torrent:get_torrent_meta_by_name(Name) of
	#torrent_meta{id = Id,
		      length = Length} ->
	    ?COUNT_REQUEST(details),
	    {ok, Binary} = torrent:get_torrent_binary(Name),
	    Parsed = benc:parse(Binary),
	    {S, L, Speed} = tracker:tracker_info(Id),
	    HTML = [{h2, [Name]},
		    {dl, [{dt, ["Size"]},
			  {dd, [util:human_length(Length)]},
			  {dt, ["Info-Hash"]},
			  {dd, [{"class", "code"}],
			   [urlencode(binary_to_list(Id))]},
			  {dt, ["Seeders"]},
			  {dd, [integer_to_list(S)]},
			  {dt, ["Leechers"]},
			  {dd, [integer_to_list(L)]},
			  {dt, ["Current total download speed"]},
			  {dd, [util:human_bandwidth(Speed)]}
			 ]},
		    {p, [{"class", "important"}],
		     ["Download ",
		      {a, [{"href", link_to_torrent(Name)},
			   {"rel", "enclosure"}],
		       [Name]}]},
		    {h2, ["Contents"]},
		    {table,
		     [{tr, [{th, ["Path"]},
			    {th, ["Size"]}]}
		      | [{tr, [{td, [FileName]},
			       {td, [util:human_length(FileLength)]}]}
			 || {FileName, FileLength} <- torrent_info:get_files(Parsed)]]},
		    {h2, ["Comments"]},
		    {'div', [{"id", "comments"}],
		     [{p, ["Sorry, I were not able to resist the urge to do this with JavaScript"]}]}
		    ],
	    Body = lists:map(fun html:to_iolist/1, HTML),
	    html_ok(Req, Body);
	not_found ->
	    ?COUNT_REQUEST(details404),
	    html_not_found(Req)
    end;

request(Req, 'GET', Path) ->
    PathLen = string:len(Path),
    case string:rstr(Path, ".torrent") of
	N when PathLen > 8, N == PathLen - 7 ->
	    Name = string:sub_string(Path, 1, PathLen - 8),
	    request(Req, 'GET', {download, Name});
	_ ->
	    request(Req, 'GET', {details, Path})
    end.

html_ok(Req, Body) ->
    Req:ok({?MIME_XHTML, html_skeleton(Body)}).

html_not_found(Req) ->
    HTML =
	[{h2, ["Not found"]},
	 {p, [{"class", "important error code"}], ["Not pirated yet."]}],
    Body = lists:map(fun html:to_iolist/1, HTML),
    Req:respond({404, [{"Content-type", ?MIME_XHTML}], html_skeleton(Body)}).


html_skeleton(Body) ->
	    [<<"<?xml version='1.0' encoding='utf-8'?>
<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN' 'DTD/xhtml1-strict.dtd'>
<html xmlns='http://www.w3.org/1999/xhtml' lang='de' xml:lang='de'>
  <head>
    <meta http-equiv='Content-Type' content='">>, ?MIME_XHTML, <<"; charset=UTF-8' />
    <title>Chaos Bay</title>
    <link rel='stylesheet' type='text/css' href='/static/chaosbay.css'/>
    <script type='text/javascript' src='/static/jquery-1.2.6.min.js'></script>
    <script type='text/javascript' src='/static/comments.js'></script>
    <link rel='alternate' type='">>, ?MIME_ATOM, <<"' title='ATOM 1.0' href='/atom' />
  </head>
  <body>
    <div id='head'>
      <p><a href='/add' id='add'>Add</a></p>
      <h1><a href='/'>Chaos Bay</a></h1>
    </div>
    <div id='content'>
">>, Body, <<"
    </div>
    <p id='foot'>
— Powered by mochiweb &amp; mnesia —
<br/>
Running on ">>,
	     case nodes(visible) of
		 [] ->
		     [];
		 VisibleNodes ->
		     [string:join([atom_to_list(Node)
				   || Node <- VisibleNodes], ", "),
		      <<" and ">>]
	     end,
	     atom_to_list(node()),
	     <<"
    </p>
  </body>
</html>">>].

link_to_details(Name) when is_binary(Name) ->
    link_to_details(binary_to_list(Name));
link_to_details(Name) ->
    "/" ++ urlencode(Name).

link_to_torrent(Name) when is_binary(Name) ->
    link_to_torrent(binary_to_list(Name));
link_to_torrent(Name) ->
    "/" ++ urlencode(Name) ++ ".torrent".

browse_link(SortName, Direction, Offset, Pattern) when is_atom(SortName) ->
    browse_link(atom_to_list(SortName), Direction, Offset, Pattern);
browse_link(SortName, asc, Offset, Pattern) ->
    browse_link(SortName, "a", Offset, Pattern);
browse_link(SortName, desc, Offset, Pattern) ->
    browse_link(SortName, "d", Offset, Pattern);
browse_link(SortName, Direction, Offset, Pattern) when is_integer(Offset) ->
    browse_link(SortName, Direction, integer_to_list(Offset), Pattern);
browse_link(SortName, Direction, Offset, Pattern) ->
    "/browse/" ++
	SortName ++ "/" ++
	Direction ++ "/" ++
	Offset ++ "/" ++
	urlencode(Pattern).
    
urlencode(S) ->
    lists:flatten(
      lists:map(fun($+) -> "%20";
		   (C) -> C
		end,
		mochiweb_util:quote_plus(S)
	       )).

build_tracker_response(Peers) ->
    [{<<"interval">>, ?TRACKER_REQUEST_INTERVAL},
     {<<"peers">>, [[{<<"peer id">>, PeerPeerId},
		     {<<"ip">>, inet_parse:ntoa(PeerIP)},
		     {<<"port">>, PeerPort}]
		    || {PeerPeerId, PeerIP, PeerPort} <- Peers]}].

build_compact_tracker_response(Peers) ->
    {Peers4, Peers6} = lists:foldl(fun({_, {_, _, _, _}, _} = Peer, {Peers4, Peers6}) ->
					   {[Peer | Peers4], Peers6};
				      ({_, {_, _, _, _, _, _, _, _}, _} = Peer, {Peers4, Peers6}) ->
					   {Peers4, [Peer | Peers6]}
				   end, {[], []}, Peers),
    [{<<"interval">>, ?TRACKER_REQUEST_INTERVAL},
     {<<"compact">>, 1},
     {<<"peers">>, list_to_binary([<<A:8, B:8, C:8, D:8, Port:16/big>>
				   || {_, {A, B, C, D}, Port} <- Peers4])},
     {<<"peers6">>, list_to_binary([<<(A bsr 8):8, (A band 16#ff):8, (B bsr 8):8, (B band 16#ff):8,
				     (C bsr 8):8, (C band 16#ff):8, (D bsr 8):8, (D band 16#ff):8,
				     (E bsr 8):8, (E band 16#ff):8, (F bsr 8):8, (F band 16#ff):8,
				     (G bsr 8):8, (G band 16#ff):8, (H bsr 8):8, (H band 16#ff):8,
				     Port:16/big>>
				    || {_, {A, B, C, D, E, F, G, H}, Port} <- Peers6])}
    ].

generate_pages(Total) ->
    {_, Result} =
	lists:foldl(fun(Offset, {N, Result}) ->
			    {N + 1, [{N, Offset} | Result]}
		    end, {1, []}, lists:seq(0, Total, ?RESULTSET_LENGTH)),
    lists:reverse(Result).

