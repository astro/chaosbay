-module(torrent_info).

-export([info_hash/1, get_length/1, get_files/1, set_tracker/1]).

info_hash(Torrent) ->
    {value, {_, _, <<Hash/binary>>}} = lists:keysearch(<<"info">>, 1, Torrent),
    Hash.


get_length(Torrent) ->
    lists:foldl(fun({_Name, Size}, Total) ->
			Total + Size
		end, 0, get_files(Torrent)).

get_files(Torrent) ->
    {value, {_, Info, _}} = lists:keysearch(<<"info">>, 1, Torrent),
    case lists:keysearch(<<"files">>, 1, Info) of
	{value, {_, Files, _}} ->
	    lists:map(
	      fun(File) ->
		      {value, {_, FilePath, _}} =
			  lists:keysearch(<<"path">>, 1, File),
		      {value, {_, FileLength, _}} =
			  lists:keysearch(<<"length">>, 1, File),
		      {FilePath, FileLength}
	      end, Files);
	_ ->
	    FileName = case lists:keysearch(<<"name">>, 1, Info) of
			   {value, {_, Name, _}} -> Name;
			   _ -> <<"Unknown">>
		       end,
	    case lists:keysearch(<<"length">>, 1, Info) of
		{value, {_, Length, _}} ->
		    [{FileName, Length}];
		_ ->
		    {value, {_, PieceLength, _}} =
			lists:keysearch(<<"piece length">>, 1, Info),
		    {value, {_, Pieces, _}} =
			lists:keysearch(<<"pieces">>, 1, Info),
		    [{FileName, (size(Pieces) div 20) * PieceLength}]
	    end
    end.


-define(TRACKER_URL, <<"http://chaosbay.hq.c3d2.de/announce">>).

set_tracker(Torrent1) ->
    Torrent2 = lists:keystore(<<"announce">>, 1, Torrent1,
			      {<<"announce">>, ?TRACKER_URL, <<0>>}),
    Torrent3 = lists:keydelete(<<"announce-list">>, 1, Torrent2),
    Torrent3.
