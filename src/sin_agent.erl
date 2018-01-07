-module(sin_agent).

-author("Michał Góra").
-vsn("1.0").

-define(INTERVAL, 30000).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([spawn/0, spawn/1]).
-export([start/0, start/1]).

-export([assign_task/2]).
-export([get_system_load/1]).
-export([cast/2]).

-include("./sin_agent.hrl").
-include("./sin_debug.hrl").
-include("./sin_system_load.hrl").

-record(state,{
  socket :: gen_tcp:socket(),
  ref :: reference(),
  secret :: reference(),
  system_load :: sin_system_load(),
  system_load_update_ref :: reference(),
  labor_office :: pid()
}).

init(Args) when erlang:is_list(Args) ->      
  ?DBG_INFO("~p ~p ~n", [?MODULE, ?FUNCTION_NAME]),
  init_loops(),
  case proplists:get_value(socket, Args) of
    undefined -> {ok, init_state(#state{})};
    Socket -> init(Socket)
  end;

init(Socket) ->
  ?DBG_INFO("sin_agent init~n"),
  init_loops(),
  {ok, init_state(#state{socket=Socket})}.

init_loops() ->
  ok.

init_state(State) ->
  State#state{
    system_load=#sin_system_load{cpu_avg_1=256, cpu_avg_5=256, cpu_avg_15=256},
    secret=erlang:make_ref()
  }.

init_remote_status_checkers() ->
  erlang:start_timer(0, self(), update_system_load, [{abs, false}]).
  
handle_cast({tcp_accept, ListenSocket, From, Ref}, State) ->
  ?DBG_INFO("~p ~p tcp_accept (1) ~p~n",[?MODULE,?FUNCTION_NAME, Ref]),
  State2 = State#state{labor_office=From},
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      ?DBG_INFO("~p ~p tcp_accept (2.1) ~p~n",[?MODULE,?FUNCTION_NAME, Ref]),
      init_remote_status_checkers(),
      gen_server:cast(From, {tcp_accepted, Ref}),
      gen_server:cast(From, {tcp_accept, ListenSocket}), % will keep the same amount of acceptors all the time
      {noreply, State2#state{socket=Socket, ref=Ref}};
    _ -> 
      ?DBG_INFO("~p ~p tcp_accept (2.2) ~p~n",[?MODULE,?FUNCTION_NAME, Ref]),
      {noreply, State2}
  end;

handle_cast({update_system_load}, State=#state{system_load_update_ref=undefined}) ->
  ?DBG_INFO("[~p:~p][{update_system_load}] ~n", [?MODULE, ?FUNCTION_NAME]),
  case State#state.socket of
    undefined -> 
      ?DBG_INFO("[~p:~p][{update_system_load}] Socket unavailable. ~n", [?MODULE, ?FUNCTION_NAME]),
      {noreply, State};
    _ -> 
      ?DBG_INFO("[~p:~p][{update_system_load}] Socket available. ~n", [?MODULE, ?FUNCTION_NAME]),
      Ref = erlang:make_ref(),
      gen_server:cast(self(), {send_via_socket, State#state.secret, {get_system_load, Ref}}),
      {noreply, State#state{system_load_update_ref=Ref}}
  end;
handle_cast({update_system_load}, State) ->
  {noreply, State};

handle_cast({message_to_task, Task, Msg}, State) ->
  ?DBG_INFO("[~p:~p][message_to_task]~n    Msg: ~p~n", [?MODULE, ?FUNCTION_NAME, Msg]),  
  gen_server:cast(self(), {send_via_socket, State#state.secret, {message_to_task, Task, Msg}}),
  {noreply, State};

handle_cast({assign_task, Task}, State) ->
  ?DBG_INFO("[~p:~p][assign_task]~n    Task: ~p~n", [?MODULE, ?FUNCTION_NAME, Task]),  
  gen_server:cast(self(), {send_via_socket, State#state.secret, {run_task, Task}}),
  {noreply, State};

handle_cast({send_modules, Modules}, State) ->
  ?DBG_INFO("[~p:~p][send_modules]~n    Modules: ~p~n", [?MODULE, ?FUNCTION_NAME, Modules]),
  Bins = sin_code:modules_to_binary(Modules),
  Bins2 = lists:filter(fun (X) -> X =/= error end, Bins),
  gen_server:cast(self(), {send_via_socket, State#state.secret, {update_modules, Bins2}}),
  {noreply, State};

handle_cast({send_via_socket, Secret, Message}, State=#state{secret=Secret}) ->
  case gen_tcp:send(State#state.socket, erlang:term_to_binary(Message)) of
    {error, closed} ->
      ?DBG_INFO("TERMINATING(1)!!!~n"),
      gen_server:stop(self(), tcp_closed, 0),
      {noreply, State};
    {error, _} ->
      {norepy, State};
    ok ->
      {noreply, State}
  end;

handle_cast(Request, State) ->
  ?DBG_INFO("handle_cast: ~p~n", [Request]),
  {noreply, State}.

handle_call({get_system_load}, _From, State) ->
  {reply, State#state.system_load, State};

handle_call(Request, _From, State) ->
  ?DBG_INFO("handle_call: ~p~n", [Request]),
  {noreply, State}.

handle_info({timeout, TimeRef, update_system_load}, State) ->
  ?DBG_INFO("[~p][loop][update_system_load] ~p~n", [?MODULE, TimeRef]),
  gen_server:cast(self(), {update_system_load}),
  TimerRef = erlang:start_timer(?INTERVAL, self(), update_system_load, [{abs, false}]),
  ?DBG_INFO("[~p][loop][update_system_load] TimerRef: ~p~n", [?MODULE, TimerRef]),
  {noreply, State};

handle_info({tcp, _Socket, MsgBin}, State) ->
  Msg = erlang:binary_to_term(MsgBin),
  ?DBG_INFO("[~p:~p] tcp: ~p~n", [?MODULE, ?FUNCTION_NAME, Msg]),
  tcp_recv(Msg, State);
  
handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
  gen_server:cast(State#state.labor_office, {agent_down, State#state.ref}),
  gen_server:stop(self(), tcp_closed, infinity),
  {stop, tcp_closed, State};

handle_info(Any, State) ->
  ?DBG_INFO("[~p:~p] any: ~p~n", [?MODULE, ?FUNCTION_NAME, Any]),
  {noreply, State}.

terminate(_Reason, _State) -> 
  ?DBG_INFO("TERMINATING!!!~n"),
  ok.

code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

% ---

spawn() ->
  ?DBG_INFO("~p ~p ~n", [?MODULE, ?FUNCTION_NAME]),
  case gen_server:start_link(?MODULE, [], []) of
    {ok, Pid} -> ?DBG_INFO("p1 ~p p2 ~p~n",[self(),Pid]), {ok, Pid};
    {error, _Error} -> error;
    ignore -> ignore
  end.

spawn(Socket) ->
  case gen_server:start_link(?MODULE, [{socket, Socket}], []) of
    {ok, Pid} -> ?DBG_INFO("p1 ~p p2 ~p~n",[self(),Pid]), {ok, Pid};
    {error, _Error} -> error;
    ignore -> ignore
  end.

start() ->
  case sin_agent:spawn() of
    {ok, Pid} ->
      {ok, #sin_agent{pid=Pid}};
    _ -> error
  end.

start(Socket) ->
case sin_agent:spawn(Socket) of
  {ok, Pid} ->
    {ok, #sin_agent{pid=Pid}};
  _ -> error
end.

% ---

tcp_recv({system_load, ReqRef, SystemLoad}, State=#state{system_load_update_ref=ReqRef}) ->
  {noreply, State#state{system_load=SystemLoad, system_load_update_ref=undefined}};

tcp_recv({message_to_proc, Target, Msg}, State) when erlang:is_pid(Target) ->
  Target ! Msg,
  {noreply, State};

tcp_recv({get_modules, Modules}, State) when erlang:is_list(Modules) ->
  gen_server:cast(self(), {send_modules, Modules}),
  {noreply, State};

tcp_recv({task_exec, started, Task}, State=#state{labor_office=Lo}) ->
  gen_server:cast(Lo, {resend_messages_for_task, Task}),
  {noreply, State};

tcp_recv({task_exec, finished, _Task, _ExecutionResult}, State) ->
  {noreply, State};

tcp_recv({task_exec, failed, _Task}, State) ->
  {noreply, State};

tcp_recv(Msg, State) ->
  ?DBG_INFO("[~p:~p/~p] ~p~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Msg]),
  {noreply, State}.

% --- API

get_system_load(AgentPid) when erlang:is_pid(AgentPid) ->
  gen_server:call(AgentPid, {get_system_load});
get_system_load(_Agent=#sin_agent{pid=Pid}) ->
  get_system_load(Pid).

assign_task(_Agent=#sin_agent{pid=Pid}, Task) ->
  gen_server:cast(Pid, {assign_task, Task}).

cast(AgentPid, Message) when erlang:is_pid(AgentPid) ->
  gen_server:cast(AgentPid, Message);
cast(_Agent=#sin_agent{pid=Pid}, Message) ->
  cast(Pid, Message).