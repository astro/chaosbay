-module(webseed).

-export([init/0, start/1, status/1]).

-record(webseed, {name,
		  status, % not_found | running | done | error
		  bytes = 0}).


init() ->
    mnesia:create_table(webseed, [{attributes, record_info(fields, webseed)}]).

start(URL) ->
    [Name1 = [_ | _] | _] = lists:reverse(string:tokens(URL, "/")),
    Name = list_to_binary(Name1),

    %% Check even before download
    case torrent:get_torrent_by_name(Name) of
	not_found ->
	    F = fun() ->
			case mnesia:read({webseed, Name}) of
			    [#webseed{status = Status}]
			    when Status =:= running;
				 Status =:= done ->
				exists;
			    _ ->
				mnesia:write(#webseed{name = Name,
						      status = running}),
				ok
			end
		end,
	    case mnesia:transaction(F) of
		{atomic, ok} ->
		    %% Spawn:
		    spawn_link(fun() ->
				  run_webseed(Name, URL)
			  end),
		    {ok, Name};
		{atomic, exists} -> exists
	    end;
	_ ->
	    exists
    end.


status(Name) ->
    {atomic, Result} =
	mnesia:transaction(
	  fun() ->
		  case mnesia:dirty_read({webseed, Name}) of
		      [] -> not_found;
		      [#webseed{status = Status}] -> Status
		  end
	  end),
    Result.


run_webseed(Name, URL) ->
    case (catch create_webseed(Name, URL)) of
	{'EXIT', Reason} ->
	    error_logger:error_msg("Error creating webseed for ~p~n~p",
				   [URL, Reason]),
	    mnesia:transaction(fun() ->
				       mnesia:write(#webseed{name = Name,
							     status = error})
			       end);
	_ ->
	    mnesia:transaction(fun() ->
				       mnesia:write(#webseed{name = Name,
							     status = done})
			       end)
    end.

create_webseed(Name, URL) ->
    {ok, ReqId} = http:request(get, {URL, []},
			       [], [{streamTo, {self, once}}]),
    receive
	{http, {ReqId, stream_start, _Headers}} ->
	    io:format("Headers: ~p~n",[_Headers])
    end,
    download_and_hash(ReqId).
    
download_and_hash(ReqId) ->
    http:stream_next(ReqId),
    receive
	{http, {ReqId, stream, BinBodyPart}} ->
	    io:format("Got ~p bytes~n",[size(BinBodyPart)]);
	{http, {ReqId, stream_end, _Headers}} ->
	    ok
    end.
