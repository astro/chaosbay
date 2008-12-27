-module(torrent).

-export([init/0, add/2, recent/1, get_torrent_by_name/1]).

-include("../include/torrent.hrl").


init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(torrent, [{attributes, record_info(fields, torrent)}]).

add(Filename, Upload) when is_list(Filename) ->
    add(list_to_binary(Filename), Upload);

add(Filename, Upload) ->
    NewFilename = case split_binary(Filename, size(Filename) - 8) of
		      {FN, <<".torrent">>} -> FN;
		      _ -> Filename
		  end,
    ParsedFile = benc:parse(Upload),
    %%Body = io_lib:format("<pre>file: ~s (~s)~n~p</pre>",[FileName, FileType, ParsedFile]),
    Id = torrent_info:info_hash(ParsedFile),
    Length = torrent_info:get_length(ParsedFile),
    ParsedFile2 = torrent_info:add_trackers(ParsedFile),
    Binary = benc:to_binary(ParsedFile2),
    Torrent = #torrent{name = NewFilename,
		       id = Id,
		       length = Length,
		       date = util:mk_timestamp(),
		       binary = Binary},
    F = fun() ->
		mnesia:write_lock_table(torrent),
		case mnesia:read({torrent, NewFilename}) of
		    [] ->
			mnesia:write(Torrent),
			ok;
		    _ ->
			exists
		end
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.


recent(Max) ->
    F = fun() ->
		S =
		    mnesia:foldl(fun(Torrent, Result) ->
					 sorted:insert(Result, Torrent)
				 end,
				 sorted:new(#torrent.date, desc, Max),
				 torrent),
		sorted:to_list(S)
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.


get_torrent_by_name(Name) when is_list(Name) ->
    get_torrent_by_name(list_to_binary(Name));

get_torrent_by_name(Name) ->
    F = fun() ->
		case mnesia:read({torrent, Name}) of
		    [Torrent] -> Torrent;
		    [] -> not_found
		end
	end,
    {atomic, Result} = mnesia:transaction(F),
    Result.
