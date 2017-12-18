-module(sin_slave_leash).

-export([init/2, init/3]).

-record(state, {
  socket :: gen_tcp:socket(),
  head_pid :: pid(),
  ref :: reference(),
  tcp_options :: [{atom(), any()}]
}).

init(HeadPid, Ref) ->
  State = #state{head_pid=HeadPid, ref=Ref, tcp_options=[]},
  find_pole(State),
  ok.

init(HeadPid, Ref, TcpOptions) when erlang:is_list(TcpOptions)->
  State = #state{head_pid=HeadPid, ref=Ref, tcp_options=TcpOptions},
  find_pole(State),
  ok.

find_pole(State) ->
  tcp_connect(State).

tcp_connect(State) ->
  case gen_tcp:connect(
    proplists:get_value(master_ip, State#state.tcp_options, {127,0,0,1}),
    proplists:get_value(master_port, State#state.tcp_options, 3456),
    [
      binary,
      {packet, 4},
      {active, false},
      {keepalive, true}
    ]
  ) of
    {ok, Socket} ->
      tcp_connect_ok(State, Socket);
    {error, _Reason} ->
      error
  end.

tcp_connect_ok(State, Socket) ->
  enter_loop(State, Socket).

enter_loop(State, Socket) ->
  NewState = State#state{socket=Socket},
  loop(NewState),
  leave_loop(NewState).

leave_loop(State) ->
  gen_tcp:close(State#state.socket).

loop(State) ->
  Socket = State#state.socket,
  receive
    {tcp, Socket, MsgBin} ->
      Msg = erlang:binary_to_term(MsgBin),
      tcp_recv(State, Msg);
    {send_to_master, Data} ->
      send_to_master(State, Data);
    _ -> loop(State)
  end.

% ---

tcp_recv(State, _Msg) ->
  loop(State).

send_to_master(State, Msg) ->
  gen_tcp:send(State#state.socket, erlang:term_to_binary(Msg)),
  loop(State).
