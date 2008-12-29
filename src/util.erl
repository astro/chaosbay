-module(util).

-export([mk_timestamp/0, human_length/1, human_duration/1, pmap/2]).

mk_timestamp() ->
    {MS, S, _} = erlang:now(),
    MS * 1000000 + S.


human_length(L) ->
    list_to_binary(human_length1(L)).

human_length1(L) when L > 1024 * 1024 * 1024 ->
    io_lib:format("~.1fG", [L / (1024 * 1024 * 1024)]);

human_length1(L) when L > 1024 * 1024 ->
    io_lib:format("~.1fM", [L / (1024 * 1024)]);

human_length1(L) ->
    io_lib:format("~.1fK", [L / 1024]).


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

