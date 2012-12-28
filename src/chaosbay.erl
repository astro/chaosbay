%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(chaosbay).
-author('author <author@example.com>').
-export([start/0, stop/0, absolute_path/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the chaosbay server.
start() ->
    chaosbay_deps:ensure(),
    ensure_started(crypto),
    ensure_started(collectd),
    setup_collectd(),
    application:start(chaosbay).

%% @spec stop() -> ok
%% @doc Stop the chaosbay server.
stop() ->
    Res = application:stop(chaosbay),
    application:stop(crypto),
    Res.


absolute_path(Path) ->
    case application:get_env(chaosbay, http_base) of
	{ok, HttpBase} ->
	    HttpBase ++ Path;
	undefined ->
	    error_logger:warning_msg("http_base has not been set~n"),
	    Path
    end.

setup_collectd() ->
    Interval = case application:get_env(chaosbay, collectd_interval) of
		   {ok, Interval1} -> Interval1;
		   undefined -> 10
	       end,
    case application:get_env(chaosbay, collectd_server) of
	{ok, Server} -> collectd:add_server(Interval, Server);
	undefined -> ignore
    end.

