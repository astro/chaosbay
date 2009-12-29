-module(graph_cache).

-export([init/0, get_graph/1]).


-record(cached, {q, result, updated = 0, waiting}).
-define(CACHE_TTL, 120).

init() ->
    mnesia:create_table(cached, [{attributes, record_info(fields, cached)}]).

get_graph(Q) ->
    I = self(),
    Now = util:mk_timestamp(),
    F = fun() ->
		case mnesia:read(cached, Q) of
		    [] ->
			mnesia:write(#cached{q = s,
					     waiting = [I]}),
			fetch;
		    [#cached{updated = Updated, result = Result}]
		    when Updated + ?CACHE_TTL > Now ->
			{result, Result};
		    [#cached{waiting = []}] ->
			mnesia:write(#cached{q = Q,
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
	    R = drop_cgi_header(os:cmd(Q)),
	    mnesia:transaction(fun() ->
				       [#cached{waiting = Waiting} = Cached] =
					   mnesia:read(cached, Q),
				       lists:foreach(fun(Waiter) when Waiter == I ->
							     ignore;
							(Waiter) ->
							     Waiter ! {Q, R}
						     end, Waiting),
				       mnesia:write(Cached#cached{waiting = [],
								  result = R,
								  updated = util:mk_timestamp()})
			       end),
	    R;
	{atomic, wait} ->
	    receive
		{Q, R} ->
		    R
	    end
    end.

drop_cgi_header([$\r, $\n, $\r, $\n | Body]) ->
    Body;
drop_cgi_header([$\n, $\n | Body]) ->
    Body;
drop_cgi_header([_ | Body]) ->
    drop_cgi_header(Body);
drop_cgi_header(_) ->
    [].
