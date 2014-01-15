-module(eric).
-export([start/1, init/1]).
-export([connect/1, join/2, nick/2, say/3]).
-record(state, {net}).

start(Config) ->
  spawn(?MODULE, init, [Config]).

init(Config) ->
  Net = eric_net:start_link(Config),
  State = #state{net=Net},
  loop(State).

loop(State = #state{net=Net}) ->
  receive
    connect ->
      Net ! {self(), connect},
      loop(State);
    {join, Channel} ->
      Net ! {self(), send, "JOIN " ++ Channel},
      loop(State);
    {nick, Nick} ->
      Net ! {self(), send, "NICK " ++ Nick},
      loop(State);
    {say, Channel, Message} ->
      Net ! {self(), send, "PRIVMSG " ++ Channel ++ " :" ++ Message},
      loop(State);
    _ ->
      unknown
  end.

connect(Client) ->
  Client ! connect.

nick(Client, Nick) ->
  Client ! {nick, Nick}.

join(Client, Channel) ->
  Client ! {join, Channel}.

say(Client, Channel, Message) ->
  Client ! {say, Channel, Message}.
