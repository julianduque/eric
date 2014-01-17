-module(eric).
-behaviour(gen_server).

%% API
-export([start/1, connect/0, join/1, nick/1, say/2]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% Records
-record(state, {net}).

%% API
start(Config) ->
  gen_server:start_link({local, eric}, ?MODULE, Config, []).

connect() ->
  gen_server:call(eric, connect).

nick(Nick) ->
  gen_server:call(eric, {nick, Nick}).

join(Channel) ->
  gen_server:call(eric, {join, Channel}).

say(Channel, Message) ->
  gen_server:call(eric, {say, Channel, Message}).

%% Callbacks

init(Config) ->
  io:format("~p~n", [Config]),
  Net = eric_net:start_link(Config),
  State = #state{net=Net},
  {ok, State}.

%%% Calls
handle_call(connect, From, State) ->
  State#state.net ! {From, connect},
  {reply, ok, State};

handle_call({join, Channel}, From, State) ->
  State#state.net ! {From, send, "JOIN " ++ Channel},
  {reply, ok, State};

handle_call({nick, Nick}, From, State) ->
  State#state.net ! {From, send, "NICK " ++ Nick},
  {reply, ok, State};

handle_call({say, Channel, Message}, From, State) ->
  State#state.net ! {From, send, "PRIVMSG " ++ Channel ++ " :" ++ Message},
  {reply, ok, State}.

%%% Casts
handle_cast(_, State) ->
  {noreply, State}.

%%% Other callbacks
handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
