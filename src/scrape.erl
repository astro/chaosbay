-module(scrape).

-export([init/0, scrape/1]).


-define(TTL, 10).
-define(TIMEOUT, 2).
-define(SCRAPE_URLs, ["http://localhost:6969/scrape",
		      "http://127.0.0.1:6969/scrape"]).
-record(scrape, {url_id, worker, last_try = 0, last_reply = 0, result, waiters = []}).

init() ->
    mnesia:create_table(scrape, [{attributes, record_info(fields, scrape)}]),
    inets:start().

%% TODO: unknown
scrape(Id) ->
    Scrapes = util:pmap(fun(ScrapeURL) ->
				scrape(ScrapeURL, Id)
			end, ?SCRAPE_URLs),
    lists:foldl(fun({ok, Seeders, Leechers}, {ok, S, _L}) when Seeders > S ->
			{ok, Seeders, Leechers};
		   ({ok, Seeders, Leechers}, unknown) ->
			{ok, Seeders, Leechers};
		   (_, R) ->
			R
		end, unknown, Scrapes).

scrape(ScrapeURL, Id) ->
    Now = util:mk_timestamp(),
    UrlId = {ScrapeURL, Id},
    F = fun() ->
		case mnesia:read({scrape, UrlId}) of
		    [] ->
			Pid = spawn_worker(ScrapeURL, Id),
			mnesia:write(#scrape{worker = Pid,
					     url_id = UrlId,
					     last_try = Now,
					     waiters = [self()]}),
			{wait, timeout};
		    [#scrape{last_reply = LastReply,
			     worker = Worker,
			     result = Result,
			     waiters = Waiters} = S]
		    when LastReply =< Now - ?TTL ->
			S2 = if
				 is_pid(Worker) ->
				     S#scrape{waiters = [self() | Waiters]};
				 true ->
				     Pid = spawn_worker(ScrapeURL, Id),
				     S#scrape{worker = Pid,
					      last_try = Now,
					      waiters = [self() | Waiters]}
			     end,
			mnesia:write(S2),
			{wait, Result};
		    [#scrape{result = Result}] ->
			{ok, Result}
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {wait, Result}} ->
	    receive
		{scraped, UrlId, Result} ->
		    Result
	    after ?TIMEOUT * 1000 ->
		    error_logger:error_msg("Waiting for ~p timed out~n", [UrlId]),
		    Result
	    end;
	{atomic, Result} ->
	    Result
    end.

spawn_worker(ScrapeURL, Id) ->
    spawn_link(fun() ->
		       worker(ScrapeURL, Id)
	       end).

worker(ScrapeURL, Id) ->
    Result = case (catch do_scrape(ScrapeURL, Id)) of
		 {'EXIT', Reason} -> Reason;
		 R -> R
	     end,
    F = fun() ->
		[#scrape{waiters = Waiters} = S] =
		    mnesia:read({scrape, {ScrapeURL, Id}}),
		mnesia:write(S#scrape{worker = none,
				      waiters = [],
				      result = Result,
				      last_reply = util:mk_timestamp()}),
		Waiters
	end,
    {atomic, Waiters} = mnesia:transaction(F),
    lists:foreach(fun(Waiter) ->
			  Waiter ! {scraped, {ScrapeURL, Id}, Result}
		  end, Waiters).
	    

do_scrape(ScrapeURL, Id) ->
    URL = ScrapeURL ++ "?info_hash=" ++ mochiweb_util:quote_plus(binary_to_list(Id)),
    {ok, {{_, 200, _}, _Attrs, Body}} = http:request(URL),
    Scraped = benc:parse(list_to_binary(Body)),
    {value, {_, Files, _}} = lists:keysearch(<<"files">>, 1, Scraped),
    {value, {_, Info, _}} = lists:keysearch(Id, 1, Files),
    {value, {_, Seeders, _}} = lists:keysearch(<<"complete">>, 1, Info),
    {value, {_, Leechers, _}} = lists:keysearch(<<"incomplete">>, 1, Info),
    {ok, Seeders, Leechers}.
