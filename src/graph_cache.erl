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
			mnesia:write(#cached{q = Q,
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
		io:format("GRAPH cached ~p~n", [Q]),
	    R;
	{atomic, fetch} ->
		io:format("GRAPH fetch ~p~n", [Q]),
	    spawn(fun() ->
			  R = list_to_binary(drop_cgi_header(run(Q))),
			io:format("GRAPH result ~p: ~B bytes~n", [Q, size(R)]),
			  mnesia:transaction(fun() ->
						     [#cached{waiting = Waiting} = Cached] =
							 mnesia:read(cached, Q),
						     lists:foreach(fun(Waiter) ->
									   Waiter ! {Q, R}
								   end, Waiting),
						     mnesia:write(Cached#cached{waiting = [],
										result = R,
										updated = util:mk_timestamp()})
					     end)
		  end),
	    receive
		{Q, R} ->
		    R
	    end;
	{atomic, wait} ->
		io:format("GRAPH wait ~p~n", [Q]),
	    receive
		{Q, R} ->
		    R
	    end
    end.

run(Cmd) ->
	process_flag(trap_exit, true),
	P = open_port({spawn, Cmd}, [stream]),
	O = run_loop(P),
	%%port_close(P),
	O.
run_loop(P) ->
	receive
		{P, {data, S}} -> S ++ run_loop(P);
		{'EXIT', P, _} -> ""
	end.

drop_cgi_header([$\r, $\n, $\r, $\n | Body]) ->
    Body;
drop_cgi_header([$\n, $\n | Body]) ->
    Body;
drop_cgi_header([_ | Body]) ->
    drop_cgi_header(Body);
drop_cgi_header(_) ->
    [].
