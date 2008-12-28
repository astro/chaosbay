%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for chaosbay.

-module(chaosbay_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/1]).

-include("../include/torrent.hrl").

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
I <b>prepend</b> our own tracker in your file. That enables me
to get numbers of Seeders &amp; Leechers faster. If you still
need a tracker, use:
</p>
<pre>http://chaosbay.congress.ccc.de:6969/announce</pre>
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
<p class='important'>→ <a href='">>, link_to_torrent(Name), <<"'>">>, Name, <<"</a></p>
<p>Then, go back to the <a href='/'>index</a> or <a href='/add'>add</a> another Torrent.</p>
">>]);
	exists ->
	    html_ok(Req, <<"
<h2>Sorry</h2>
<p class='important'>I already have this file.</p>
<p>You might want to rename it if the contents are really different.</p>
">>)
    end;

request(Req, 'GET', "") ->
    Torrents = torrent:recent(50),
    TorrentsScraped = torrents_with_scrapes(Torrents),
    HTML =
	{table, [{"border", "1"}],
	 [{tr, [{th, ["Name"]},
		{th, [""]},
		{th, ["Size"]},
		{th, ["Added"]},
		{th, ["S"]},
		{th, ["L"]}
	       ]}
	  | lists:map(fun({#torrent{name = Name,
				    length = Length,
				    date = Date}, S, L, Class}) ->
			      Link = link_to_torrent(Name),
			      {tr, [{"class", Class}],
			       [{td, [Name]},
				{td, [{a, [{"href", Link}],
				       ["Get"]}]},
				{td, [util:human_length(Length)]},
				{td, [util:human_duration(util:mk_timestamp() - Date)]},
				{td, [S]},
				{td, [L]}
			       ]}
		      end, TorrentsScraped)]},
    Body = html:to_iolist(HTML),
    html_ok(Req, Body);
		   
request(Req, 'GET', "static/" ++ Path) ->
    DocRoot = chaosbay_deps:local_path(["priv", "www"]),
    Req:serve_file(Path, DocRoot);

request(Req, 'GET', Path) ->
    PathLen = string:len(Path),
    case string:rstr(Path, ".torrent") of
	N when N == PathLen - 7 ->
	    Name = string:sub_string(Path, 1, PathLen - 8),
	    case torrent:get_torrent_by_name(Name) of
		#torrent{binary = Binary} ->
		    Req:ok({"application/x-bittorrent",
			    Binary});
		not_found ->
		    Req:not_found()
	    end;
	_ ->
	    Req:not_found()
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
  </head>
  <body>
    <div id='head'>
      <p><a href='/add'>Add</a></p>
      <h1>Chaos Bay</h1>
    </div>
    <div id='content'>
">>, Body, <<"
    </div>
  </body>
</html>">>]}).

link_to_torrent(Name) when is_binary(Name) ->
    link_to_torrent(binary_to_list(Name));
link_to_torrent(Name) ->
"/" ++ mochiweb_util:quote_plus(Name) ++ ".torrent".

torrents_with_scrapes(Torrents) ->
    util:pmap(
      fun(#torrent{id = Id} = Torrent) ->
	      {S, L, Class} = case scrape:scrape(Id) of
				  {ok, 0, 0} ->
				      {"0", "0", "dead"};
				  {ok, 0, L1} ->
				      {"0",
				       integer_to_list(L1),
				       "starving"};
				  {ok, S1, L1} ->
				      {integer_to_list(S1),
				       integer_to_list(L1),
				       ""};
				  _ ->
				      {"?", "?", "dead"}
			      end,
	      {Torrent, S, L, Class}
      end, Torrents).
