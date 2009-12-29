-module(http_cache).

-export([init/0, get_url/1]).


-record(cached, {url, result, updated = 0, waiting}).
-define(CACHE_TTL, 120).

init() ->
    mnesia:create_table(cached, [{attributes, record_info(fields, cached)}]).

get_url(URL) ->
    I = self(),
    Now = util:mk_timestamp(),
    F = fun() ->
		case mnesia:read(cached, URL) of
		    [] ->
			mnesia:write(#cached{url = URL,
					     waiting = [I]}),
			fetch;
		    [#cached{updated = Updated, result = Result}]
		    when Updated + ?CACHE_TTL > Now ->
			{result, Result};
		    [#cached{waiting = []}] ->
			mnesia:write(#cached{url = URL,
					     waiting = [I]}),
			fetch;
		    [#cached{waiting = Waiting} = Cached] ->
			mnesia:write(Cached#cached{waiting = [I | Waiting]}),
			wait
		end
	end,
    TR = mnesia:transaction(F),
    case TR of
	{atomic, {result, R}} ->
	    R;
	{atomic, fetch} ->
	    R = case catch http:request(URL) of
		    {ok, {{_, 200, _}, _, Body}} ->
			{ok, Body};
		    E ->
			io:format("GET ~p: ~p~n", [URL, E]),
			error
		end,
	    mnesia:transaction(fun() ->
				       [#cached{waiting = Waiting} = Cached] =
					   mnesia:read(cached, URL),
				       lists:foreach(fun(Waiter) when Waiter == I ->
							     ignore;
							(Waiter) ->
							     Waiter ! {URL, R}
						     end, Waiting),
				       mnesia:write(Cached#cached{waiting = [],
								  result = R,
								  updated = util:mk_timestamp()})
			       end),
	    R;
	{atomic, wait} ->
	    receive
		{URL, R} ->
		    R
	    end
    end.
