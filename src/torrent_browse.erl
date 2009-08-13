-module(torrent_browse).

-export([search/5]).


-include("../include/torrent.hrl").

search(Pattern, Max, Offset, SortField, SortDir) ->
    Now = util:mk_timestamp(),
    FilterFun = build_filter(string:to_lower(Pattern)),
    Fields = lists:map(fun string:to_lower/1,
		       lists:map(fun atom_to_list/1,
				 record_info(fields, browse_result))),
    SortN = case util:list_index(SortField, Fields) of
		0 -> exit(no_such_field);
		N -> N + 1
	    end,
    Sorted = torrent_search:fold(
	       fun(#torrent_meta{name = Name,
				 id = Id,
				 length = Length,
				 date = Date},
		   Sorted) ->
		       NameLower = string:to_lower(binary_to_list(Name)),
		       case FilterFun(NameLower) of
			   true ->
			       Age = Now - Date,
			       {S, L, Speed} = tracker:tracker_info(Id),
			       Comments = comment:get_comments_count(Name),
			       Result = #browse_result{name = Name,
						       id = Id,
						       length = Length,
						       comments = Comments,
						       age = Age,
						       seeders = S,
						       leechers = L,
						       speed = Speed},
			       sorted:insert(Sorted, Result);
			   false ->
			       Sorted
		       end
	       end, sorted:new(SortN, SortDir, Max + Offset)),
    Result = lists:nthtail(Offset, sorted:to_list(Sorted)),
    {Result, sorted:get_total(Sorted)}.

build_filter("") ->
    fun(_) -> true end;
build_filter([$\s | Pattern]) ->
    build_filter(Pattern);
build_filter(Pattern) ->
    {Expr, Rest} = lists:splitwith(fun($\s) -> false;
				      (_) -> true
				   end, Pattern),
    fun(Name) ->
	    case (build_filter(Rest))(Name) of
		true ->
		    %% Check
		    case string:str(Name, Expr) of
			0 -> false;
			_ -> true
		    end;
		false ->
		    false
	    end
    end.
	    
    
