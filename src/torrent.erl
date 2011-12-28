-module(torrent).

-export([init/0,
	 add/2, add_http/1, add_from_dir/1,
	 recent/1,
	 get_torrent_meta_by_name/1,
	 get_torrent_meta_by_id/1, torrent_name_by_id_t/1,
	 get_torrent_binary/1]).

-include("../include/torrent.hrl").


init() ->
	ok.

add_http(URL) ->
    [Filename | _] = lists:reverse(string:tokens(URL, "/")),
    {ok, {{_, 200, _}, _Attrs, Body}} = http:request(URL),
    add(Filename, list_to_binary(Body)).
add_from_dir(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    lists:map(fun(File) ->
			  FileLen = string:len(File),
			  case string:rstr(File, ".torrent") of
			      N when FileLen > 8, N == FileLen - 7 ->
				  Name = string:sub_string(File, 1, FileLen - 8),
				  io:format("Reading ~p~n",[Dir ++ "/" ++ File]),
				  {ok, Bin} = file:read_file(Dir ++ "/" ++ File),
				  R = add(Name, Bin),
				  {File, R};
			      _ -> {File, ignored}
			  end
		  end, Files).

add(Filename, _Upload) when Filename == ""; Filename == <<>> ->
    invalid;

add(Filename, Upload) when is_list(Filename) ->
    add(list_to_binary(Filename), Upload);

add(Filename, Upload) ->
    NewFilename = case split_binary(Filename, size(Filename) - 8) of
		      {FN, <<".torrent">>} -> FN;
		      _ -> Filename
		  end,
    case (catch benc:parse(Upload)) of
	{'EXIT', _Reason} -> {error, "Cannot parse this file"};
	ParsedFile ->
	    %%Body = io_lib:format("<pre>file: ~s (~s)~n~p</pre>",[FileName, FileType, ParsedFile]),
	    Id = torrent_info:info_hash(ParsedFile),
	    Length = torrent_info:get_length(ParsedFile),
	    ParsedFile2 = torrent_info:set_tracker(ParsedFile),
	    Binary = benc:to_binary(ParsedFile2),
		Timestamp = util:mk_timestamp(),
		C = sql_conns:request_connection(),
		{ok, _Columns, Rows} = pgsql:equery(C, "select infohash from torrents where infohash = $1 or name = $2", 
												[Id, NewFilename]),
		case Rows of
			[] ->
				pgsql:equery(C, "insert into torrents (infohash, name, length, data, timestamp) values ($1, $2, $3, $4, $5)", 
						[Id, NewFilename, Length, Binary, Timestamp]),
				Result = {ok, NewFilename};
			_ ->
				Result = exists
		end,
		sql_conns:release_connection(C),
		Result
    end.

recent(Max) ->
	C = sql_conns:request_connection(),
	{ok, _Columns, Rows} = 
		pgsql:equery(C, "select (name, infohash, length, timestamp) from torrents order by timestamp asc limit $1", [Max]),
	sql_conns:release_connection(C),
	Result = lists:flatmap(fun(X) ->
								{{Name, InfoHash, Length, Timestamp}} = X,
								[#torrent_meta{ name = Name, id = InfoHash, length = Length, date = Timestamp}]
							end, Rows),
	Result.

get_torrent_meta_by_name(Name) when is_list(Name) ->
    get_torrent_meta_by_name(list_to_binary(Name));

get_torrent_meta_by_name(Name) ->
	C = sql_conns:request_connection(),
	case pgsql:equery(C, "select (name, infohash, length, timestamp) from torrents where name = $1", [Name]) of
		{ok, _, [{E} | _]} -> 
			{Name, Id, Length, Date} = E,
			Result = #torrent_meta{name = Name, id = Id, length = Length, date = Date};
		_ -> 
			Result = not_found
	end,
	sql_conns:release_connection(C),
    Result.

get_torrent_meta_by_id(Id) ->
	C = sql_conns:request_connection(),
	case pgsql:equery(C, "select (name, infohash, length, timestamp) from torrents where infohash = $1", [Id]) of
		{ok, _, [{E} | _]} -> 
			{Name, Id, Length, Date} = E,
			Result = {ok, #torrent_meta{name = Name, id = Id, length = Length, date = Date}};
		_ -> 
			Result = not_found
	end,
	sql_conns:release_connection(C),
    Result.

torrent_name_by_id_t(Id) ->
	C = sql_conns:request_connection(),
	case pgsql:equery(C, "select (name) from torrents where infohash = $1", [Id]) of
		{ok, _, [{Name} | _]} -> 
			Result = {ok,Name};
		_ -> 
			Result = not_found
	end,
	sql_conns:release_connection(C),
	Result.


get_torrent_binary(Name) when is_list(Name) ->
    get_torrent_binary(list_to_binary(Name));
get_torrent_binary(Name) ->
	C = sql_conns:request_connection(),
	case pgsql:equery(C, "select (data) from torrents where name = $1", [Name]) of
		{ok, _, [{E} | _]} -> 
			Result = {ok, E};
		_ -> 
			Result = not_found
	end,
	sql_conns:release_connection(C),
	Result.
