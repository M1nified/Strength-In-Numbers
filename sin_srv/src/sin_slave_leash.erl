-module(sin_slave_leash).

-export([init/2, init/3]).

-include("./sin_task.hrl").

-record(state, {
  socket :: gen_tcp:socket(),
  head_pid :: pid(),
  head_ref :: reference(),
  tcp_options :: [{atom(), any()}]
}).

init(HeadPid, Ref) ->
  init_elements(),
  State = #state{head_pid=HeadPid, head_ref=Ref, tcp_options=[]},
  find_pole(State),
  ok.

init(HeadPid, Ref, TcpOptions) when erlang:is_list(TcpOptions)->
  init_elements(),
  State = #state{head_pid=HeadPid, head_ref=Ref, tcp_options=TcpOptions},
  find_pole(State),
  ok.

init_elements() ->
  sin_system_load:start().    

find_pole(State) ->
  tcp_connect(State).

tcp_connect(State) ->
  io:format("[~p:~p/~p] Slave is looking for the Master...~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  case gen_tcp:connect(
    proplists:get_value(master_ip, State#state.tcp_options, {127,0,0,1}),
    proplists:get_value(master_port, State#state.tcp_options, 3456),
    [
      binary,
      {packet, 4},
      {active, true},
      {keepalive, true}
    ]
  ) of
    {ok, Socket} ->
      tcp_connect_ok(State, Socket);
    {error, _Reason} ->
      error
  end.

tcp_connect_ok(State, Socket) ->
  io:format("[~p:~p/~p] Slave found the Master.~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY]),
  enter_loop(State, Socket).

enter_loop(State, Socket) ->
  NewState = State#state{socket=Socket},
  loop(NewState),
  leave_loop(NewState).

leave_loop(State) ->
  gen_tcp:close(State#state.socket).

loop(State) ->
  io:format("[~p:~p][loop] State: ~p~n", [?MODULE, ?FUNCTION_NAME, State]),
  Socket = State#state.socket,
  receive
    {tcp, Socket, MsgBin} ->
      Msg = erlang:binary_to_term(MsgBin),
      tcp_recv(Msg, State);
    {send_to_master, Data} ->
      send_to_master(State, Data);
    Any ->
      io:format("[~p:~p] Msg: ~p~n", [?MODULE, ?FUNCTION_NAME, Any]), 
      loop(State)
  end.

% ---

tcp_recv({get_system_load, ReqRef}, State) ->
  io:format("[get_system_load] Master asked for system load update.~n"),
  SystemLoad = sin_system_load:get_system_load(),
  io:format("[get_system_load] System load: ~p~n", [SystemLoad]),
  SendResult = gen_tcp:send(State#state.socket, erlang:term_to_binary({system_load, ReqRef, SystemLoad})),
  io:format("[get_system_load] SendResult: ~p~n", [SendResult]),
  loop(State);

tcp_recv({run_task, Task}, State) ->
  loop(State);

tcp_recv({update_modules, Modules}, State) when erlang:is_list(Modules) ->
  loop(State);

tcp_recv(Msg, State) ->
  io:format("[~p:~p/~p][any] Msg: ~p~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Msg]),
  loop(State).

send_to_master(State, Msg) ->
  gen_tcp:send(State#state.socket, erlang:term_to_binary(Msg)),
  loop(State).
