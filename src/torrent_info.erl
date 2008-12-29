-module(torrent_info).

-export([info_hash/1, get_length/1, add_trackers/1]).

info_hash(Torrent) ->
    {value, {_, _, <<Hash/binary>>}} = lists:keysearch(<<"info">>, 1, Torrent),
    Hash.


get_length(Torrent) ->
    {value, {_, Info, _}} = lists:keysearch(<<"info">>, 1, Torrent),
    case lists:keysearch(<<"files">>, 1, Info) of
	{value, {_, Files, _}} ->
	    lists:foldl(
	      fun(File, Length) ->
		      {value, {_, FileLength, _}} =
			  lists:keysearch(<<"length">>, 1, File),
		      Length + FileLength
	      end, 0, Files);
	_ ->
	    case lists:keysearch(<<"length">>, 1, Info) of
		{value, {_, Length, _}} ->
		    Length;
		_ ->
		    {value, {_, PieceLength, _}} =
			lists:keysearch(<<"piece length">>, 1, Info),
		    {value, {_, Pieces, _}} =
			lists:keysearch(<<"pieces">>, 1, Info),
		    (size(Pieces) div 20) * PieceLength
	    end
    end.

-define(TRACKER_URLS, [<<"udp://81.163.2.20:6969/announce">>,
		       <<"udp://81.163.2.24:6969/announce">>,
		       <<"udp://81.163.2.23:6969/announce">>,
		       <<"http://81.163.2.20:6969/announce">>,
		       <<"http://81.163.2.24:6969/announce">>,
		       <<"http://81.163.2.23:6969/announce">>,
		       <<"dht://">>]).

add_trackers(Torrent) ->
    add_trackers1(Torrent, ?TRACKER_URLS).

add_trackers1(Torrent, [URL1 | _] = URLs) ->
    Tracker1 =
	case lists:keysearch(<<"announce">>, 1, Torrent) of
	    {value, {_, A, _}} -> A;
	    _ -> ""
	end,
    Trackers1 =
	case lists:keysearch(<<"announce-list">>, 1, Torrent) of
	    {value, {_, L, _}} ->
		L;
	    _ -> []
	end,
    %%io:format("Old annonuces: ~p~n", [[[Tracker1] | Trackers1]]),
    Trackers = [[URL] || URL <- URLs] ++
	[[Tracker1] | Trackers1],
    %%io:format("New annonuces: ~p~n", [Trackers]),
    Torrent2 = lists:keystore(<<"announce">>, 1, Torrent,
			      {<<"announce">>, URL1, <<0>>}),
    Torrent3 = lists:keystore(<<"announce-list">>, 1, Torrent2,
			      {<<"announce-list">>, Trackers, <<0>>}),
    Torrent3.
