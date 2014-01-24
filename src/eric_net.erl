-module(eric_net).
-behaviour(gen_server).

-export([start_link/1, connect/0, send/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("eric.hrl").

-record(state, {nick,
                username,
                realname,
                host,
                port,
                socket,
                connected=false,
                secure
               }).

start_link(Config) ->
  gen_server:start_link({local, eric_net}, ?MODULE, Config, []).

init(Config) ->
  Nick = eric_config:get(nick, "eric", Config),
  Username = eric_config:get(username, Config),
  Realname = eric_config:get(realname, Config),
  Host = eric_config:get(host, Config),
  Port = eric_config:get(port, 6667, Config),
  Secure = eric_config:get_bool(secure, false, Config),
  State = #state{nick=Nick, username=Username, realname=Realname, host=Host, port=Port, secure=Secure},
  {ok, State}.

% Public API
connect() ->
  gen_server:call(eric_net, connect).

send(Data) ->
  gen_server:call(eric_net, {send, Data}).

stop() ->
  gen_server:cast(eric_net, stop).

% Callbacks
handle_call(connect, _Ref, State) ->
  Return = case State#state.connected of
             true -> {reply, ok, State};
             false ->
               Socket = connect(State),
               NewState = State#state{socket=Socket, connected=true},
               {reply, ok, NewState}
           end,
  Return;

handle_call({send, Data}, _Ref, State) ->
  Network = get_network(State),
  send(Network, State#state.socket, Data),
  {reply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({_, Socket, Data}, State) ->
  NewState = State#state{socket=Socket},
  handle_receive(Data),
  {noreply, NewState};

handle_info({tcp_closed, _Socket},  State) ->
  {noreply, State};

handle_info({ssl_closed, _Socket}, State) ->
  {noreply,  State};

handle_info(Unknown, State) ->
  io:format("Unknown message: ~p~n", [Unknown]),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

% Private
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
      send(Network, Socket, "USER " ++ State#state.username ++ " * * " ++ State#state.realname),
      Socket;
    {error, Reason} ->
      io:format("Error: ~p", [Reason]),
      error
  end.

send(Network, Socket, Data) ->
  Network:send(Socket, Data ++ ?CRLF).

handle_receive(Data) ->
  Client = whereis(eric),
  Client ! eric_parser:parse(Data).
