-module(torrent_browse).

-export([search/5]).


-include("../include/torrent.hrl").

% TODO: description as erlang docu
% Input
% 	Pattern = Search pattern name, as list
% 	Max = Max results (integer)
% 	Offset = Offset (integer)
% 	SortField = sort field as list. May contain name, length, age, comments, seeders, leechers, speed
% 	SortDir = atom (asc, desc)
% Output
%   list of #browse_result records
search(Pattern, Max, Offset, SortField, SortDir) ->
    Now = util:mk_timestamp(),
	SanatizedSortField = sanatize_sortField(SortField),
	C = sql_conns:request_connection(),
	case Pattern of 
		E when is_list(E), E =/= [] ->
			SQLSearchPattern = "%" ++ E ++ "%",	
			SQLStatement = "select (name, infohash, length, count_comments(name), $1 - timestamp, count_seeders(infohash), count_leechers(infohash), count_downspeed(infohash)) from torrents "++ 
							"where name ilike $2 order by " ++ SanatizedSortField ++ " " ++ atom_to_list(SortDir) ++ 
							" limit $3 offset $4",
			{_, _, MatchingTorrents} = pgsql:equery(C, SQLStatement, [Now, SQLSearchPattern, Max, Offset]);
		_ ->
			SQLStatement = "select (name, infohash, length, count_comments(name), $1 - timestamp, count_seeders(infohash), count_leechers(infohash), count_downspeed(infohash)) from torrents order by "
	   							++ SanatizedSortField ++ " " ++ atom_to_list(SortDir) ++	" limit $2 offset $3", 
		{_, _, MatchingTorrents} = pgsql:equery(C, SQLStatement, [Now, Max, Offset])
	end,
	sql_conns:release_connection(C),
	Result = lists:flatmap(fun(X) -> 
					{{Name, InfoHash, Length, CommentCount, Age, Seeders, Leechers, Downspeed}} = X, 
					[#browse_result{
						name = Name,
						id = InfoHash,
						length = Length,
						comments = CommentCount,
						age = Age,
						seeders = Seeders,
						leechers = Leechers,
						speed = Downspeed}]
				  end, 
			MatchingTorrents),
	{Result, length(Result)}.


sanatize_sortField(SortField) when SortField =:= "age" ->
	"timestamp";
sanatize_sortField(SortField) when SortField =:= "comments" ->
	"count_comments(name)";
sanatize_sortField(SortField) when SortField =:= "leechers" ->
	"count_seeders(infohash)";
sanatize_sortField(SortField) when SortField =:= "seeders" ->
	"count_seeders(infohash)";
sanatize_sortField(SortField) when SortField =:= "speed" ->
	"count_downspeed(infohash)";
sanatize_sortField(SortField) when SortField =:= "length" ->
	"length";
sanatize_sortField(SortField) when SortField =:= "name" ->
	"name";
sanatize_sortField(_)  ->
	"timestamp".
