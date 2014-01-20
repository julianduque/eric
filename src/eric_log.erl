-module(eric_log).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init(State) ->
  {ok, State}.

handle_event({response, [_, _, ping, Host]}, State) ->
  eric:send("PING :" ++ binary_to_list(Host)),
  {ok, State};

handle_event({response, [From, _, privmsg, To, Message]}, State) ->
  io:format("[~s] <~s> ~s~n", [color:yellowb(binary_to_list(To)), 
                               binary_to_list(From),  
                               binary_to_list(Message)]),
  {ok, State};

handle_event({response, Data}, State) ->
  io:format("[~s] ~p~n", [color:greenb("info"), Data]),
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
