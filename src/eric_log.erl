-module(eric_log).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init(State) ->
  {ok, State}.

% Ping Pong
handle_event({response, [_, _, ping, Host]}, State) ->
  eric:send("PING :" ++ binary_to_list(Host)),
  {ok, State};

handle_event({response, [_, _, pong, _, _]}, State) ->
  {ok, State};

% PrivMsg
handle_event({response, [From, _, privmsg, To, Message]}, State) ->
  io:format("[~s] <~s> ~s~n", [color:yellowb(binary_to_list(To)), 
                               binary_to_list(From),
                               binary_to_list(Message)]),
  {ok, State};

% JOIN
handle_event({response, [From, _, join, Channel, _]}, State) ->
  io:format("[~s] ~s has joined ~s", [color:blueb("join"), 
                                             color:whiteb(binary_to_list(From)),
                                             color:whiteb(binary_to_list(Channel))]),
  {ok, State};

% PART
handle_event({response, [From, _, part, Channel, _]}, State) ->
  io:format("[~s] ~s left ~s", [color:blueb("part"), 
                                             color:whiteb(binary_to_list(From)),
                                             color:whiteb(binary_to_list(Channel))]),
  {ok, State};


% JOIN
handle_event({response, [From, _, quit, Msg]}, State) ->
  io:format("[~s] ~s has quit - ~s ~n", [color:blueb("quit"), 
                                             binary_to_list(From),
                                             color:whiteb(binary_to_list(Msg))]),
  {ok, State};

% TOPIC
handle_event({response, [_, _, rpl_topic, _, Channel, Topic]}, State) ->
  print_topic(Channel, Topic),
  {ok, State};

handle_event({response, [From, _, topic, Channel, Topic]}, State) ->
  print_topic(From, Channel, Topic),
  {ok, State};

% NAMES
handle_event({response, [_, _, rpl_namreply, _, _, Channel, Names]}, State) ->
  io:format("[~s] ~s: ~s ~n", [color:blueb("names"), 
                              color:whiteb(binary_to_list(Channel)),
                              binary_to_list(Names)]),
  {ok, State};

handle_event({response, [_, _, rpl_endofnames, _, Channel, Msg]}, State) ->
  io:format("[~s] ~s: ~s ~n", [color:blueb("names"), 
                           color:whiteb(binary_to_list(Channel)),
                           binary_to_list(Msg)]),
  {ok, State};

% NOTICE
handle_event({response, [_, _, notice, _, Msg]}, State) ->
  print_notice(Msg),
  {ok, State};

% MODE
handle_event({response, [_, _, mode, Nick, Mode]}, State) ->
  io:format("[~s] ~s sets mode ~s ~n", [color:yellowb("mode"), binary_to_list(Nick), binary_to_list(Mode)]),
  {ok, State};

% MOTD
handle_event({response, [_, _, rpl_motdstart, _, Msg]}, State) ->
  print_motd(Msg),
  {ok, State};

handle_event({response, [_, _, rpl_motd, _, Msg]}, State) ->
  print_motd(Msg),
  {ok, State};

handle_event({response, [_, _, rpl_endofmotd, _, Msg]}, State) ->
  print_motd(Msg),
  {ok, State};

% Everything else
handle_event({response, Data}, State) ->
  io:format("[~s] ~p ~n", [color:greenb("info"), Data]),
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

% Helper funcs
print_motd(Msg) ->
  io:format("[~s] ~s ~n", [color:magentab("motd"), binary_to_list(Msg)]).

print_notice(Msg) ->
  io:format("[~s] ~s ~n", [color:greenb("notice"), binary_to_list(Msg)]).

print_topic(Channel, Topic) ->
  io:format("[~s] ~s: ~s ~n", [color:blueb("topic"),
                                 color:whiteb(binary_to_list(Channel)),
                                 binary_to_list(Topic)]).
print_topic(From, Channel, Topic) ->
  io:format("[~s] ~s by ~s: ~s  ~n", [color:blueb("topic"),
                                 color:whiteb(binary_to_list(Channel)),
                                 color:whiteb(binary_to_list(From)),
                                 binary_to_list(Topic)]).
