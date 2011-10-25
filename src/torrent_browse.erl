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
% 	Name (binary),
% 	InfoHash (binary),
% 	Length (integer),
% 	Age (integer),
% 	comments (integer),
% 	seeders (integer),
% 	leechers (integer),
% 	speed (integer)
search(Pattern, Max, Offset, SortField, SortDir) ->
	io:format("search,Args: ~w ~w ~w ~w ~w~n",[Pattern, Max, Offset, SortField, SortDir]),
    Now = util:mk_timestamp(),
	C = sql_conns:request_connection(),
	{_, _, E} = pgsql:equery(C, "select (name, infohash, length, timestamp) from torrents", []),
	Result = lists:flatmap(fun(X) -> 
					{{Name, InfoHash, Length, Timestamp}} = X, 
					{_, _, [{Comments}]} = pgsql:equery(C, "select count(*) from comments where name = $1", [Name]),
					[#browse_result{
						name = Name,
						id = InfoHash,
						length = Length,
						comments = Comments,
						age = Now - Timestamp,
						seeders = 0,
						leechers = 0,
						speed = 0}]
				  end, 
			E),
	sql_conns:release_connection(C),
	{Result, length(Result)}.
