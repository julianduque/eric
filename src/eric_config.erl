-module(eric_config).
-export([load/1, get/2, get/3, get_bool/2, get_bool/3]).

%% Public
load(File) ->
  case file:consult(File) of
    {ok, [Config | _]} -> Config;
    {error, _Reason} ->
      error
  end.

get_bool(Property, Config) ->
  proplists:get_bool(Property, Config).

get_bool(Property, Default, Config) ->
  with_default(get_bool(Property, Config), Default).

get(Property, Config) ->
  proplists:get_value(Property, Config).

get(Property, Default, Config) ->
  with_default(get(Property, Config), Default).

%% Private 
with_default(undefined, Default) ->
  Default;
with_default(Value, _) ->
  Value.
