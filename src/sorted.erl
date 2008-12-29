-module(sorted).

-export([new/3, to_list/1, get_total/1, insert/2]).
-export([compare/4]).


-record(sorted, {n = 1,
		 dir = asc,
		 max = undefined,
		 list = [],
		 total = 0}).

new(N, Dir, Max) ->
    #sorted{n = N,
	    dir = Dir,
	    max = Max}.

to_list(#sorted{list = L}) ->
    L.

get_total(#sorted{total = Total}) ->
    Total.

insert(#sorted{n = N,
	       dir = Dir,
	       max = Max,
	       list = L,
	       total = Total} = S, E) ->
    L2 = insert1(N, Dir, L, E),
    L3 = if
	     is_integer(Max) andalso length(L2) > Max ->
		 lists:sublist(L2, Max);
	     true ->
		 L2
	 end,
    S#sorted{list = L3, total = Total + 1}.

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
    A1 = element(N, A),
    B1 = element(N, B),
    case Dir of
	desc -> A1 < B1;
	asc -> A1 > B1
    end.

	    
