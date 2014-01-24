-module(eric_net_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Config) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

init(Config) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 5,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,
  EricNet = {eric_net, {eric_net, start_link, [Config]}, 
             Restart, Shutdown, Type, [eric_net]},
  {ok, {SupFlags, [EricNet]}}.
