-module(eric).
-behaviour(gen_server).

%% API
-export([start/1, start/2, stop/0, connect/0, send/1, join/1, nick/1, msg/2, whois/1, quit/0, quit/1]).
-export([say/2, names/1, part/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%% API
start(Config) ->
  start(Config, eric_log).

start(Config, ResponseHandler) ->
  Server = gen_server:start_link({local, eric}, ?MODULE, Config, []),
  gen_event:start({local, response_handler}),
  gen_event:add_handler(response_handler, ResponseHandler, []),
  Server.

stop() ->
  gen_server:cast(eric, stop).

connect() ->
  gen_server:call(eric, connect).

send(Data) ->
  gen_server:call(eric, {send, Data}).

nick(Nick) ->
  gen_server:call(eric, {nick, Nick}).

join(Channel) ->
  gen_server:call(eric, {join, Channel}).

part(Channel) ->
  gen_server:call(eric, {part, Channel}).

msg(Channel, Message) ->
  gen_server:call(eric, {msg, Channel, Message}).

say(Channel, Message) ->
  msg(Channel, Message).

whois(Nick) ->
  gen_server:cast(eric, {whois, Nick}).

quit() ->
  gen_server:call(eric, {quit, []}).

quit(Message) ->
  gen_server:call(eric, {quit, Message}).

names(Channel) ->
  gen_server:call(eric, {names, Channel}).

%% Callbacks
init(Config) ->
  eric_net:start_link(Config),
  {ok, []}.

%%% Calls
handle_call(connect, _Ref, State) ->
  eric_net:connect(),
  {reply, ok, State};

handle_call({send, Data}, _Ref, State) ->
  eric_net:send(Data),
  {reply, ok, State};

handle_call({join, Channel}, _Ref, State) ->
  eric_net:send("JOIN " ++ Channel),
  {reply, ok, State};

handle_call({part, Channel}, _Ref, State) ->
  eric_net:send("PART " ++ Channel),
  {reply, ok, State};

handle_call({nick, Nick}, _Ref, State) ->
  eric_net:send("NICK " ++ Nick),
  {reply, ok, State};

handle_call({msg, Channel, Message}, _Ref, State) ->
  eric_net:send("PRIVMSG " ++ Channel ++ " :" ++ Message),
  {reply, ok, State};

handle_call({names, Channel}, _Ref, State) ->
  eric_net:send("NAMES " ++ Channel),
  {reply, ok, State};

handle_call({quit, Message}, _Ref, State) ->
  eric_net:send("QUIT :" ++ Message),
  {reply, ok, State}.

%%% Casts
handle_cast({whois, Nick}, State) ->
  eric_net:send("WHOIS " ++ Nick),
  {noreply, State};

handle_cast(stop, State) ->
  quit(),
  {stop, normal, State};

handle_cast(_, State) ->
  {noreply, State}.

%%% Other callbacks
handle_info({response, Data}, State) ->
  gen_event:notify(response_handler, {response, Data}),
  {noreply, State};

handle_info(_, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
