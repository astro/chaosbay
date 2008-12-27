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
<form action='/add' method='POST' enctype='multipart/form-data'>
<input type='file' name='file'/>
<input type='submit' value='Add'/>
</form>
">>,
    html_ok(Req, Body);

request(Req, 'POST', "add") ->
    Multipart = mochiweb_multipart:parse_form(Req),
    {FileName, {_FileType, _}, File} = proplists:get_value("file", Multipart),
    ok = torrent:add(FileName, File),
    html_ok(Req, "Ok");

request(Req, 'GET', "") ->
    Torrents = torrent:recent(50),
    TorrentsScraped = torrents_with_scrapes(Torrents),
    HTML =
	{table, [{"border", "1"}],
	 [{tr, [{th, ["Name"]},
		{th, [""]},
		{th, ["Added"]},
		{th, ["Size"]},
		{th, ["S"]},
		{th, ["L"]}
	       ]}
	  | lists:map(fun({#torrent{name = Name,
				    length = Length,
				    date = Date}, S, L, Class}) ->
			      Link = "/" ++ mochiweb_util:quote_plus(binary_to_list(Name)) ++ ".torrent",
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
