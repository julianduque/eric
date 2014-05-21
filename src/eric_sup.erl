-module(eric_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Config).

init(Config) ->
    Server = {eric, {eric, start, [Config]},
      permanent, 2000, worker, [eric]},
    Children = [Server],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.
