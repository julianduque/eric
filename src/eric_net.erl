-module(eric_net).
-export([start/1, start_link/1]).
-export([init/1]).

-include("eric.hrl").

-record(state, {nick,
                username,
                realname,
                host,
                port,
                socket,
                client,
                connected=false
               }).

start(Config) ->
  spawn(?MODULE, init, [Config]).

start_link(Config) ->
  spawn_link(?MODULE, init, [Config]).

init(Config) ->
  Nick = eric_config:get(nick, "eric", Config),
  Username = eric_config:get(username, Config),
  Realname = eric_config:get(realname, Config),
  Host = eric_config:get(host, Config),
  Port = eric_config:get(port, 6667, Config),
  State = #state{nick=Nick, username=Username, realname=Realname, host=Host, port=Port},
  loop(State).

loop(State = #state{}) ->
  receive
    {Client, connect} ->
      case State#state.connected of
        true ->
          loop(State);
        false ->
          NewState = State#state{client=Client, connected=true},
          connect(NewState),
          loop(NewState)
      end;
    {send, Data} ->
      send(State#state.socket, Data),
      loop(State);
    {Client, send, Data} ->
      send(State#state.socket, Data),
      loop(State#state{client=Client});
    {tcp, Socket, Data} ->
      NewState = State#state{socket=Socket},
      handle_receive(Data, NewState),
      loop(NewState);
    {tcp_closed, _Socket} ->
      ok;
    Unknown ->
      io:format("Unknown ~p~n", [Unknown]),
      loop(State)
  end.

connect(State) ->
  case gen_tcp:connect(State#state.host,
                       State#state.port,
                       [binary, {active, true}]) of
    {ok, Socket} ->
      send(Socket, "NICK " ++ State#state.nick),
      send(Socket, "USER " ++ State#state.username ++ " * * "
                           ++ State#state.realname);
    {error, Reason} ->
      io:format("Error: ~p", [Reason]),
      error
  end.

send(Socket, Data) ->
  gen_tcp:send(Socket, Data ++ ?CRNL).

handle_receive(Data, State) ->
  Client = State#state.client,
  Client ! {response, binary_to_list(Data)}.
