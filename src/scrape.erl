-module(scrape).

-export([init/0, scrape/2]).


-define(TTL, 600).
-define(TIMEOUT, 2).  %% Web sites must actually be responsive at any cost
-define(STARVE_TIMEOUT, 20).
-record(scrape, {url_id, worker, last_try = 0, last_reply = 0, result, waiters = []}).

init() ->
    mnesia:create_table(scrape, [{attributes, record_info(fields, scrape)}]),
    inets:start().

scrape(AnnounceList, Id) ->
    Scrapes = util:pmap(fun(ScrapeURL) ->
				scrape1(ScrapeURL, Id)
			end, lists:filter(fun(URL) -> is_list(URL) end,
					  lists:map(fun announce_to_scrape_url/1,
						    AnnounceList))),
    lists:foldl(fun({ok, Seeders, Leechers, Downloaded}, {ok, S, _L, _D}) when Seeders > S ->
			{ok, Seeders, Leechers, Downloaded};
		   ({ok, Seeders, Leechers, Downloaded}, unknown) ->
			{ok, Seeders, Leechers, Downloaded};
		   (_, R) ->
			R
		end, unknown, Scrapes).

announce_to_scrape_url(Bin) when is_binary(Bin) ->
    announce_to_scrape_url(binary_to_list(Bin));

announce_to_scrape_url("http://" ++ URL1) ->
    case lists:splitwith(fun(C) -> C =/= $/ end, URL1) of
	{HostPart, "/announce" ++ _} ->
	    "http://" ++ HostPart ++ "/scrape";
	_ ->
	    false
    end;
announce_to_scrape_url(_URL) ->
    false.
    

scrape1(ScrapeURL, Id) ->
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
    Result = case (catch util:timeout(fun() ->
					      catch do_scrape(ScrapeURL, Id)
				      end, (?STARVE_TIMEOUT + 1) * 1000)) of
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
	    

do_scrape("http://" ++ _ = ScrapeURL, Id) ->
    URL = ScrapeURL ++ "?info_hash=" ++ mochiweb_util:quote_plus(binary_to_list(Id)),
    error_logger:info_msg("HTTP GET ~s~n", [URL]),
    {ok, {{_, 200, _}, _Attrs, Body}} = http:request(get, {URL, []},
						     [], [{timeout, ?STARVE_TIMEOUT}]),
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
