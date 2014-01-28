-module(eric_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Config = eric_config:load("secure.config"),
    eric_sup:start_link(Config).

stop(_State) ->
    ok.
