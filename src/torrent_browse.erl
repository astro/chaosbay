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
	io:format("search,Args: ~w ~w ~w ~w ~w~n",[Pattern, Max, Offset, SortField, SortDir]),
    Now = util:mk_timestamp(),
	C = sql_conns:request_connection(),
	MatchingTorrents = ordered_search(C, Pattern, Now, Max, Offset, SortField, SortDir),
	sql_conns:release_connection(C),
	Result = lists:flatmap(fun(X) -> 
					{{Name, InfoHash, Length, CommentCount, Age}} = X, 
					[#browse_result{
						name = Name,
						id = InfoHash,
						length = Length,
						comments = CommentCount,
						age = Age,
						seeders = 0,
						leechers = 0,
						speed = 0}]
				  end, 
			MatchingTorrents),
	{Result, length(Result)}.

% 	SortField = sort field as list. May contain name, length, age, comments, seeders, leechers, speed
ordered_search(C, [], Now, Max, Offset, "name", asc) ->
	{_, _, MatchingTorrents} = pgsql:equery(C, 
		"select (name, infohash, length, count_comments(name), $1 - timestamp) from torrents order by name asc limit $2 offset $3", 
		[Now, Max, Offset]),
		MatchingTorrents;
ordered_search(C, [], Now, Max, Offset, "name", desc) ->
	{_, _, MatchingTorrents} = pgsql:equery(C, 
		"select (name, infohash, length, count_comments(name), $1 - timestamp) from torrents order by name desc limit $2 offset $3", 
		[Now, Max, Offset]),
		MatchingTorrents;

ordered_search(C, [], Now, Max, Offset, "length", asc) ->
	{_, _, MatchingTorrents} = pgsql:equery(C, 
		"select (name, infohash, length, count_comments(name), $1 - timestamp) from torrents order by length asc limit $2 offset $3", 
		[Now, Max, Offset]),
		MatchingTorrents;
ordered_search(C, [], Now, Max, Offset, "length", desc) ->
	{_, _, MatchingTorrents} = pgsql:equery(C, 
		"select (name, infohash, length, count_comments(name), $1 - timestamp) from torrents order by length desc limit $2 offset $3", 
		[Now, Max, Offset]),
		MatchingTorrents;
	
ordered_search(C, [], Now, Max, Offset, "age", asc) ->
	{_, _, MatchingTorrents} = pgsql:equery(C, 
		"select (name, infohash, length, count_comments(name), $1 - timestamp) from torrents order by timestamp asc limit $2 offset $3", 
		[Now, Max, Offset]),
		MatchingTorrents;
ordered_search(C, [], Now, Max, Offset, "age", desc) ->
	{_, _, MatchingTorrents} = pgsql:equery(C, 
		"select (name, infohash, length, count_comments(name), $1 - timestamp) from torrents order by timestamp desc limit $2 offset $3", 
		[Now, Max, Offset]),
		MatchingTorrents;

ordered_search(C, [], Now, Max, Offset, "comments", asc) ->
	{_, _, MatchingTorrents} = pgsql:equery(C, 
		"select (name, infohash, length, count_comments(name), $1 - timestamp) from torrents order by count_comments(name) asc limit $2 offset $3", 
		[Now, Max, Offset]),
		MatchingTorrents;
ordered_search(C, [], Now, Max, Offset, "comments", desc) ->
	{_, _, MatchingTorrents} = pgsql:equery(C, 
		"select (name, infohash, length, count_comments(name), $1 - timestamp) from torrents order by count_comments(name) desc limit $2 offset $3", 
		[Now, Max, Offset]),
		MatchingTorrents;

ordered_search(C, [], Now, Max, Offset, _SortField, SortDir) ->
	ordered_search(C, [], Now, Max, Offset, "name", SortDir);

ordered_search(C, Pattern, Now, Max, Offset, "name", asc) ->
			SearchPattern = "%" ++ Pattern ++ "%",	
			{_, _, MatchingTorrents} = pgsql:equery(C, 
				"select (name, infohash, length, count_comments(name) as comments, $1 - timestamp as age) from torrents where name ilike $2 order by name asc limit $3 offset $4", [Now, SearchPattern, Max, Offset]),
			MatchingTorrents;
ordered_search(C, Pattern, Now, Max, Offset, "name", desc) ->
			SearchPattern = "%" ++ Pattern ++ "%",	
			{_, _, MatchingTorrents} = pgsql:equery(C, 
				"select (name, infohash, length, count_comments(name) as comments, $1 - timestamp as age) from torrents where name ilike $2 order by name desc limit $3 offset $4", [Now, SearchPattern, Max, Offset]),
			MatchingTorrents.
