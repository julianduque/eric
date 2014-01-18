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
                connected=false,
                secure
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
  Secure = eric_config:get_bool(secure, false, Config),
  State = #state{nick=Nick, username=Username, realname=Realname, host=Host, port=Port, secure=Secure},
  loop(State).

loop(State = #state{}) ->
  Network = get_network(State),
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
      send(Network, State#state.socket, Data),
      loop(State);
    {Client, send, Data} ->
      send(Network, State#state.socket, Data),
      loop(State#state{client=Client});
    {tcp, Socket, Data} ->
      NewState = State#state{socket=Socket},
      handle_receive(Data, NewState),
      loop(NewState);
    {ssl, Socket, Data} ->
      NewState = State#state{socket=Socket},
      handle_receive(Data, NewState),
      loop(NewState);
    {tcp_closed, _Socket} ->
      ok;
    {ssl_closed, _Socket} ->
      ok;
    Unknown ->
      io:format("Unknown ~p~n", [Unknown]),
      loop(State)
  end.

get_network(State) ->
  Network = case State#state.secure of
              true ->
                ssl:start(),
                ssl;
              false ->
                gen_tcp
            end,
  Network.

connect(State) ->
  Network = get_network(State),
  case Network:connect(State#state.host,
                       State#state.port,
                       [binary, {active, true}, {packet, line}, {keepalive, true}]) of
    {ok, Socket} ->
      send(Network, Socket, "NICK " ++ State#state.nick),
      send(Network, Socket, "USER " ++ State#state.username ++ " * * " ++ State#state.realname);
    {error, Reason} ->
      io:format("Error: ~p", [Reason]),
      error
  end.

send(Network, Socket, Data) ->
  Network:send(Socket, Data ++ ?CRNL).

handle_receive(Data, State) ->
  Client = State#state.client,
  Client ! {response, binary_to_list(Data)}.
