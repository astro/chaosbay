%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for chaosbay.

-module(chaosbay_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/1]).

-include("../include/torrent.hrl").
-include("../include/comment.hrl").

-define(TRACKER_REQUEST_INTERVAL, 60).

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
    io:format("~s ~s~n", [Method, Path]),
    Path2 = lists:dropwhile(fun(C) -> C == $/ end,
			    Path),
    request(Req, Method, Path2).


%% Internal API

request(Req, 'GET', "add") ->
    Body = <<"
<h2>Add a .torrent file</h2>
<form action='/add' method='POST' enctype='multipart/form-data' class='important'>
  <input type='file' name='file' accept='application/x-bittorrent' maxlength='524288'/>
  <input type='submit' value='Add'/>
</form>
<h3>Tracker information will be added automatically!</h2>
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
</p>
">>,
    html_ok(Req, Body);

request(Req, 'POST', "add") ->
    Multipart = mochiweb_multipart:parse_form(Req),
    {FileName, {_FileType, _}, File} = proplists:get_value("file", Multipart),
    case torrent:add(FileName, File) of
	{ok, Name} ->
	    html_ok(Req, [<<"
<h2>Very good!</h2>
<p>Now please download our .torrent file and seed that!</p>
<p class='important'><a href='">>, link_to_torrent(Name), <<"'>">>, Name, <<"</a></p>
<p>Then, view <a href='">>, link_to_details(Name), <<"'>details</a>, go back to the <a href='/'>index</a> or <a href='/add'>add</a> another Torrent.</p>
">>]);
	exists ->
	    html_ok(Req, <<"
<h2>Sorry</h2>
<p class='important'>I already have this file.</p>
<p>You might want to rename it if the contents are really different.</p>
">>)
    end;

request(Req, 'GET', "") ->
    Torrents = torrent:recent(200),
    HTML =
	[{img, [{"src", "/static/chaosbay.png"}], []},
	 {table, [{"border", "1"}],
	  [{tr, [{th, ["Name"]},
		 {th, [""]},
		 {th, ["Size"]},
		 {th, ["Age"]},
		 {th, [{"title", "Comments"}],
		  ["C"]},
		 {th, [{"title", "Seeders"}],
		  ["S"]},
		 {th, [{"title", "Leechers"}],
		  ["L"]}
		]}
	   | lists:map(fun(#torrent{name = Name,
				    id = Id,
				    length = Length,
				    date = Date}) ->
			       {S, L} = tracker:tracker_info(Id),
			       Class = case {S, L} of
					   {0, 0} -> "dead";
					   {0, _} -> "starving";
					   {_, _} -> ""
				       end,
			       C = comment:get_comments_count(Name),
			       LinkDetails = link_to_details(Name),
			       LinkTorrent = link_to_torrent(Name),
			       {tr, [{"class", Class}],
				[{td, [{a, [{"href", LinkDetails}],
					[Name]}]},
				 {td, [{a, [{"href", LinkTorrent},
					    {"class", "download"}],
					["Get"]}]},
				 {td, [util:human_length(Length)]},
				 {td, [util:human_duration(util:mk_timestamp() - Date)]},
				 {td, [{"style", if
						     C == 0 -> "color: #aaa";
						     true -> "font-weight:bold"
						 end}],
				  [integer_to_list(C)]},
				 {td, [integer_to_list(S)]},
				 {td, [integer_to_list(L)]}
				]}
		       end, Torrents)]}],
    Body = lists:map(fun html:to_iolist/1, HTML),
    html_ok(Req, Body);
		   
request(Req, 'GET', "static/" ++ Path) ->
    DocRoot = chaosbay_deps:local_path(["priv", "www"]),
    Req:serve_file(Path, DocRoot, [{"Cache-Control", "max-age=7200"},
				   {"Expires", "Thu, 30 Oct 2008 23:42:59 GMT"}]);

request(Req, Method, "comments/" ++ Name)
  when Method =:= 'GET';
       Method =:= 'POST' ->

    if
	Method =:= 'POST' ->
	    Posted = Req:parse_post(),
	    {value, {_, Text}} = lists:keysearch("text", 1, Posted),
	    comment:add(Name, Text);
	true -> no_post
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
    QS = Req:parse_qs(),
    io:format("announce from ~p: ~p~n", [Req:get(peer), QS]),
    {value, {_, InfoHash1}} = lists:keysearch("info_hash", 1, QS),
    InfoHash = list_to_binary(InfoHash1),
    {value, {_, PeerId1}} = lists:keysearch("peer_id", 1, QS),
    PeerId = list_to_binary(PeerId1),
    Reply =
	case lists:keysearch("event", 1, QS) of
	    {value, {_, "stopped"}} ->
		tracker:tracker_request_stopped(InfoHash, PeerId),
		[{<<"ok">>, <<"true">>, <<0>>}];
	    
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
		IP = list_to_binary(Req:get(peer)),
		Result =
		    tracker:tracker_request(InfoHash, PeerId,
					    IP, Port,
					    Uploaded, Downloaded, Left),
		case Result of
		    not_found ->
			[{<<"failure reason">>, <<"No torrent registered for info_hash">>, <<0>>}];
		    {peers, Peers} ->
			[{<<"interval">>, ?TRACKER_REQUEST_INTERVAL, <<0>>},
			 {<<"peers">>, [[{<<"peer id">>, PeerPeerId, <<0>>},
					 {<<"ip">>, PeerIP, <<0>>},
					 {<<"port">>, PeerPort, <<0>>}]
					|| {PeerPeerId, PeerIP, PeerPort} <- Peers],
			  <<0>>}]
		end
	end,
    error_logger:info_msg("Announce reply: ~p~n",[Reply]),
    Bencoded = benc:to_binary(Reply),
    Req:ok({"application/x-bittorrent",
	    Bencoded});


request(Req, 'GET', {download, Name}) ->
    case torrent:get_torrent_by_name(Name) of
	#torrent{binary = Binary} ->
	    Req:ok({"application/x-bittorrent",
		    Binary});
	not_found ->
	    Req:not_found()
    end;

request(Req, 'GET', {details, Name}) ->
    case torrent:get_torrent_by_name(Name) of
	#torrent{id = Id,
		 length = Length,
		 binary = Binary} ->
	    Parsed = benc:parse(Binary),
	    {S, L} = tracker:tracker_info(Id),
	    HTML = [{h2, [Name]},
		    {dl, [{dt, ["Size"]},
			  {dd, [util:human_length(Length)]},
			  {dt, ["Info-Hash"]},
			  {dd, [{"class", "code"}],
			   [mochiweb_util:quote_plus(binary_to_list(Id))]},
			  {dt, ["Seeders"]},
			  {dd, [integer_to_list(S)]},
			  {dt, ["Leechers"]},
			  {dd, [integer_to_list(L)]}]},
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
	    Req:not_found()
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
    Req:ok({"text/html",
	    [<<"<?xml version='1.0' encoding='utf-8'?>
<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN' 'DTD/xhtml1-strict.dtd'>
<html xmlns='http://www.w3.org/1999/xhtml' lang='de' xml:lang='de'>
  <head>
    <meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />
    <title>Chaos Bay</title>
    <link rel='stylesheet' type='text/css' href='/static/chaosbay.css'/>
    <script type='text/javascript' src='/static/jquery-1.2.6.min.js'></script>
    <script type='text/javascript' src='/static/comments.js'></script>
  </head>
  <body>
    <div id='head'>
      <p><a href='/add'>Add</a></p>
      <h1><a href='/'>Chaos Bay</a></h1>
    </div>
    <div id='content'>
">>, Body, <<"
    </div>
    <p id='foot'>
— Powered by mochiweb &amp; opentracker —
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
</html>">>]}).

link_to_details(Name) when is_binary(Name) ->
    link_to_details(binary_to_list(Name));
link_to_details(Name) ->
    "/" ++ mochiweb_util:quote_plus(Name).

link_to_torrent(Name) when is_binary(Name) ->
    link_to_torrent(binary_to_list(Name));
link_to_torrent(Name) ->
    "/" ++ mochiweb_util:quote_plus(Name) ++ ".torrent".

tracker_stats_for_torrents(Torrents) ->
    lists:map(fun(#torrent{id = Id} = Torrent) ->
		      {S, L} = tracker:tracker_info(Id),
		      {Torrent, S, L}
	      end, Torrents).
