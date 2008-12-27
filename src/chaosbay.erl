%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(chaosbay).
-author('author <author@example.com>').
-export([start/0, stop/0]).

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
    application:start(chaosbay).

%% @spec stop() -> ok
%% @doc Stop the chaosbay server.
stop() ->
    Res = application:stop(chaosbay),
    application:stop(crypto),
    Res.
