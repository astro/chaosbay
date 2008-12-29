-module(scrape).

-export([init/0, scrape/1]).


-define(TTL, 10).
-define(TIMEOUT, 2).
-define(SCRAPE_URLs, ["http://81.163.2.20:6969/scrape",
		      "http://81.163.2.24:6969/scrape",
		      "http://81.163.2.23:6969/scrape"
		     ]).
-record(scrape, {url_id, worker, last_try = 0, last_reply = 0, result, waiters = []}).

init() ->
    mnesia:create_table(scrape, [{attributes, record_info(fields, scrape)}]),
    inets:start().

%% TODO: unknown
scrape(Id) ->
    Scrapes = util:pmap(fun(ScrapeURL) ->
				scrape(ScrapeURL, Id)
			end, ?SCRAPE_URLs),
    lists:foldl(fun({ok, Seeders, Leechers, Downloaded}, {ok, S, _L, _D}) when Seeders > S ->
			{ok, Seeders, Leechers, Downloaded};
		   ({ok, Seeders, Leechers, Downloaded}, unknown) ->
			{ok, Seeders, Leechers, Downloaded};
		   (_, R) ->
			R
		end, unknown, Scrapes).

scrape(ScrapeURL, Id) ->
    I = self(),
    Now = util:mk_timestamp(),
    UrlId = {ScrapeURL, Id},
    F = fun() ->
		case mnesia:read({scrape, UrlId}) of
		    [] ->
			mnesia:write(#scrape{worker = true,
					     url_id = UrlId,
					     last_try = Now,
					     waiters = [I]}),
			{start_wait, timeout};
		    [#scrape{last_reply = LastReply,
			     worker = Worker,
			     result = Result,
			     waiters = Waiters} = S]
		    when LastReply =< Now - ?TTL ->
			if
			    Worker =:= true ->
				mnesia:write(S#scrape{waiters = [I | Waiters]}),
				{wait, Result};
			    true ->
				mnesia:write(S#scrape{worker = true,
						      last_try = Now,
						      waiters = [I | Waiters]}),
				{start_wait, Result}
			end;
		    [#scrape{result = Result}] ->
			Result
		end
	end,
    case mnesia:transaction(F) of
	{atomic, {Do, Result}}
	when Do =:= start_wait;
	     Do =:= wait ->
	    if
		Do =:= start_wait ->
		    spawn_worker(ScrapeURL, Id);
		true -> already_started
	    end,
	    receive
		{scraped, UrlId, NewResult} ->
		    NewResult
	    after ?TIMEOUT * 1000 ->
		    error_logger:error_msg("Waiting for ~p timed out~n", [UrlId]),
		    Result
	    end;
	{atomic, Result} ->
	    Result
    end.

spawn_worker(ScrapeURL, Id) ->
    spawn(fun() ->
		  worker(ScrapeURL, Id)
	  end).

worker(ScrapeURL, Id) ->
    Result = case (catch do_scrape(ScrapeURL, Id)) of
		 {'EXIT', Reason} ->
		     error_logger:error_msg("Scrape failed: ~p~n", [Reason]),
		     Reason;
		 R ->
		     error_logger:info_msg("Scraped: ~p~n", [R]),
		     R
	     end,
    F = fun() ->
		[#scrape{waiters = Waiters} = S] =
		    mnesia:read({scrape, {ScrapeURL, Id}}),
		mnesia:write(S#scrape{worker = false,
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
    case lists:keysearch(Id, 1, Files) of
	{value, {_, Info, _}} ->
	    {value, {_, Seeders, _}} = lists:keysearch(<<"complete">>, 1, Info),
	    {value, {_, Leechers, _}} = lists:keysearch(<<"incomplete">>, 1, Info),
	    {value, {_, Downloaded, _}} = lists:keysearch(<<"downloaded">>, 1, Info),
	    {ok, Seeders, Leechers, Downloaded};
	false -> {ok, 0, 0, 0}
    end.
