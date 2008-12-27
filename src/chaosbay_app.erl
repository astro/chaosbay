%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the chaosbay application.

-module(chaosbay_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for chaosbay.
start(_Type, _StartArgs) ->
    chaosbay_deps:ensure(),
    chaosbay_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for chaosbay.
stop(_State) ->
    ok.
