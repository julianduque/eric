-module(custom_reply).
-behaviour(gen_event).

-export([start/1, init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

start(Config) ->
  eric:start(Config, custom_reply),
  eric:connect(),
  eric:join("#eric-test"),
  eric:msg("#eric-test", "Hey!").

init([]) ->
  {ok, []}.

handle_event({response, Data}, State) ->
  io:format("[~s] ~s", [color:yellowb("custom"), Data]),
  {ok, State};

handle_event(_, State) ->
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
