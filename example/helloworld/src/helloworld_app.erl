%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the helloworld application.

-module(helloworld_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for helloworld.
start(_Type, _StartArgs) ->
    helloworld_deps:ensure(),
    Sup = helloworld_sup:start_link(),
    webmachine_config:db_init(),
    webmachine_config:start(),
    webmachine_mod:start(),
    webmachine_mod:start_modules(),
    Sup.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for helloworld.
stop(_State) ->
    webmachine_mod:stop_modules(),
    ok.
