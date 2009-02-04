-module(util).

-export([mk_timestamp/0, mk_timestamp_us/0,
	 human_length/1, human_bandwidth/1, human_duration/1,
	 timestamp_to_iso8601/1,
	 pmap/2, timeout/2, safe_mnesia_create_table/2]).

mk_timestamp() ->
    {MS, S, _} = erlang:now(),
    MS * 1000000 + S.

mk_timestamp_us() ->
    {MS, S, SS} = erlang:now(),
    (MS * 1000000 + S) * 1000000 + SS.


-define(UPPER_READABLE_LIMIT, 1024).

divide_until_readable(I) ->
    divide_until_readable1(I, ["", "K", "M", "G", "T", "P", "E"]).

divide_until_readable1(I, [U]) ->
    {I, U};
divide_until_readable1(I, [_ | R]) when I > ?UPPER_READABLE_LIMIT ->
    divide_until_readable1(I / 1024, R);
divide_until_readable1(I, [U | _]) ->
    {I, U}.


human_length(L) ->
    {I, U} = divide_until_readable(L),
    S = io_lib:format("~.1f~sB", [I / 1.0, U]),
    list_to_binary(S).

human_bandwidth(B) ->
    {I, U} = divide_until_readable(B),
    S = io_lib:format("~.1f~sB/s", [I / 1.0, U]),
    list_to_binary(S).


human_duration(D) ->
    list_to_binary(human_duration1(D)).

human_duration1(D) when D >= 24 * 60 * 60->
    io_lib:format("~Bd ago", [D div (24 * 60 * 60)]);

human_duration1(D) when D >= 60 * 60->
    io_lib:format("~Bh", [D div (60 * 60)]);

human_duration1(D) when D >= 60 ->
    io_lib:format("~Bm", [D div 60]);

human_duration1(D) ->
    io_lib:format("~Bs", [D]).


timestamp_to_iso8601(TS) ->
    Now = {TS div 1000000, TS rem 1000000, 0},
    {{Y, M, D}, {Hour, Min, Sec}} = calendar:now_to_universal_time(Now),
    list_to_binary(
      io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B+00:00",
		    [Y, M, D, Hour, Min, Sec])).


%% http://yarivsblog.com/articles/2008/02/08/the-erlang-challenge/
pmap(Fun, List) ->
    Parent = self(),
    Pids = [spawn_link(fun() ->
			       Parent ! {self(), Fun(Elem)}
		       end)
	    || Elem <- List],
    [receive
	 {'EXIT', _From, Reason} -> exit(Reason);
	 {Pid, Val} -> Val
     end
     || Pid <- Pids].


timeout(Fun, Timeout) ->
    I = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun() ->
			     Result = Fun(),
			     I ! {Ref, Result}
		     end),
    receive
	{Ref, Result} ->
	    Result
    after Timeout ->
	    exit(Pid, timeout),
	    exit(timeout)
    end.


safe_mnesia_create_table(Name, TabDef) ->
    case mnesia:create_table(Name, TabDef) of
	{atomic, ok} ->
	    ok;
	{aborted, {already_exists, Name}} ->
	    error_logger:info_msg("Picked up existing database ~p", [Name]),
	    %% TODO: check attributes
	    ok;
	E -> exit(E)
    end.
