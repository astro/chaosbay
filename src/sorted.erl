-module(sorted).

-export([new/3, to_list/1, insert/2]).


-record(sorted, {n = 1,
		 dir = asc,
		 max = undefined,
		 list = []}).

new(N, Dir, Max) ->
    #sorted{n = N,
	    dir = Dir,
	    max = Max}.

to_list(#sorted{list = L}) ->
    L.

insert(#sorted{n = N,
	       dir = Dir,
	       max = Max,
	       list = L}, E) ->
    L2 = insert1(N, Dir, L, E),
    L3 = if
	     is_integer(Max) andalso length(L2) > Max ->
		 lists:sublist(L2, Max);
	     true ->
		 L2
	 end,
    #sorted{list = L3}.

insert1(_, _, [], E) ->
    [E];

insert1(N, Dir, [L1 | L], E) ->
    case compare(N, Dir, L1, E) of
	true ->
	    [E, L1 | L];
	false ->
	    [L1 | insert1(N, Dir, L, E)]
    end.

compare(N, Dir, A, B) ->
    A1 = element(N + 1, A),
    B1 = element(N + 1, B),
    case Dir of
	asc -> A1 < B1;
	desc -> A1 > B1
    end.

	    
