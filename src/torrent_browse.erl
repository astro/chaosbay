-module(torrent_browse).

-export([search/5]).


-include("../include/torrent.hrl").

search(Pattern, Max, Offset, SortN, SortDir) ->
    FilterFun = build_filter(Pattern),
    Sorted = torrent_search:fold(fun(#torrent_meta{name = Name} = TorrentMeta, Sorted) ->
					 case FilterFun(Name) of
					     true ->
						 sorted:insert(Sorted, TorrentMeta);
					     false ->
						 Sorted
					 end
				 end, sorted:new(SortN, SortDir, Max + Offset)),
    sorted:to_list(Sorted).

build_filter("") ->
    fun(_) -> true end;
build_filter([$\s | Pattern]) ->
    build_filter(Pattern);
build_filter(Pattern) ->
    {Expr, Rest} = lists:splitwith(fun($\s) -> false;
				      (_) -> true
				   end, Pattern),
    {ok, RE} = re:compile(regexp:sh_to_awk("*" ++ Expr ++ "*"), [unicode, caseless]),
    fun(Name) ->
	    case (build_filter(Rest))(Name) of
		true ->
		    %% Check
		    case re:run(Name, RE) of
			nomatch -> false;
			{match, _} -> true
		    end;
		false ->
		    false
	    end
    end.
	    
    
