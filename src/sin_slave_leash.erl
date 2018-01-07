-module(sin_slave_leash).

-author("Michał Góra").
-vsn("1.0").

-export([init/2, init/3]).

-include("./sin_debug.hrl").
-include("./sin_task.hrl").

-record(state, {
  socket :: gen_tcp:socket(),
  head_pid :: pid(),
  head_ref :: reference(),
  tcp_options :: [{atom(), any()}],
  module_orders :: [{reference(), [module()]}]
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
  Ip = proplists:get_value(ip, State#state.tcp_options, {127,0,0,1}),
  Port = proplists:get_value(port, State#state.tcp_options, 3456),
  io:format("[~p:~p/~p] Slave is looking for the Master (~p:~p)...~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Ip, Port]),
  case gen_tcp:connect(
    Ip,
    Port,
    [
      binary,
      {packet, 4},
      {active, true},
      {keepalive, true}
    ]
  ) of
    {ok, Socket} ->
      tcp_connect_ok(State, Socket);
    {error, Reason} ->
      io:format("[~p:~p/~p] Error: ~p~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Reason]),
      tcp_connect(State),
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
  ?DBG_INFO("[~p:~p][loop] State: ~p~n", [?MODULE, ?FUNCTION_NAME, State]),
  Socket = State#state.socket,
  receive
    {tcp, Socket, MsgBin} ->
      Msg = erlang:binary_to_term(MsgBin),
      tcp_recv(Msg, State);
    {send_to_master, Data} ->
      send_to_master(State, Data);
    {tcp_closed, Socket} ->
      ?DBG_INFO("[~p:~p][loop] tcp_closed ~n", [?MODULE, ?FUNCTION_NAME]),
      tcp_connect(State);
    Any ->
      ?DBG_INFO("[~p:~p] Msg: ~p~n", [?MODULE, ?FUNCTION_NAME, Any]), 
      loop(State)
  end.

% ---

tcp_recv({get_system_load, ReqRef}, State) ->
  ?DBG_INFO("[get_system_load] Master asked for system load update.~n"),
  SystemLoad = sin_system_load:get_system_load(),
  ?DBG_INFO("[get_system_load] System load: ~p~n", [SystemLoad]),
  SendResult = gen_tcp:send(State#state.socket, erlang:term_to_binary({system_load, ReqRef, SystemLoad})),
  ?DBG_INFO("[get_system_load] SendResult: ~p~n", [SendResult]),
  loop(State);

tcp_recv(Req={message_to_task, _Task, Msg}, State=#state{head_pid=HeadPid, head_ref=HeadRef}) ->
  ?DBG_INFO("[~p:~p][message_to_task]~n    Msg: ~p~n", [?MODULE, ?FUNCTION_NAME, Msg]),
  gen_server:cast(HeadPid, {sin_slave_leash, HeadRef, Req}),
  loop(State);

tcp_recv(Req={run_task, Task}, State=#state{head_pid=HeadPid, head_ref=HeadRef}) ->
  ?DBG_INFO("[~p:~p][run_task]~n    Task: ~p~n", [?MODULE, ?FUNCTION_NAME, Task]),
  gen_server:cast(HeadPid, {sin_slave_leash, HeadRef, Req}),
  loop(State);  

tcp_recv(Req={update_modules, Modules}, State=#state{head_pid=HeadPid, head_ref=HeadRef}) when erlang:is_list(Modules) ->
  ?DBG_INFO("[~p:~p][update_modules]~n", [?MODULE, ?FUNCTION_NAME]),
  gen_server:cast(HeadPid, {sin_slave_leash, HeadRef, Req}),
  loop(State);

tcp_recv(Msg, State) ->
  ?DBG_INFO("[~p:~p/~p][any] Msg: ~p~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Msg]),
  loop(State).

send_to_master(State, Msg) ->
  gen_tcp:send(State#state.socket, erlang:term_to_binary(Msg)),
  loop(State).

% ---
