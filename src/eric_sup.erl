-module(eric_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Config) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Config).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Config) ->
  {
   ok,
   {
     {
      one_for_one,
      1000,
      3600
     },
     [
      {eric, {eric, start, [Config]}, permanent, 2000, worker, [eric]}
     ]
    }
  }.
