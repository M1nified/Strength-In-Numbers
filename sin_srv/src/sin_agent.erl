-module(sin_agent).

-define(INTERVAL, 30000).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([spawn/0, spawn/1]).
-export([start/0, start/1]).

-export([assign_task/2]).
-export([get_system_load/1]).

-include("./sin_agent.hrl").
-include("./sin_system_load.hrl").

-record(state,{
  socket :: gen_tcp:socket(),
  ref :: reference(),
  system_load :: sin_system_load(),
  system_load_update_ref :: reference()
}).

init(Args) when erlang:is_list(Args) ->      
  io:format("~p ~p ~n", [?MODULE, ?FUNCTION_NAME]),
  init_loops(),
  case proplists:get_value(socket, Args) of
    undefined -> {ok, init_state(#state{})};
    Socket -> init(Socket)
  end;

init(Socket) ->
  io:format("sin_agent init~n"),
  init_loops(),
  {ok, init_state(#state{socket=Socket})}.

init_loops() ->
  erlang:start_timer(0, self(), update_system_load, [{abs, false}]).

init_state(State) ->
  State#state{
    system_load=#sin_system_load{cpu_avg_1=256, cpu_avg_5=256, cpu_avg_15=256}
  }.
  
handle_cast({tcp_accept, ListenSocket, From, Ref}, State) ->
  io:format("~p ~p tcp_accept (1) ~p~n",[?MODULE,?FUNCTION_NAME, Ref]),
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      io:format("~p ~p tcp_accept (2.1) ~p~n",[?MODULE,?FUNCTION_NAME, Ref]),
      gen_server:cast(From, {tcp_accepted, Ref}),
      gen_server:cast(From, {tcp_accept, ListenSocket}), % will keep the same amount of acceptors all the time
      {noreply, State#state{socket=Socket, ref=Ref}};
    _ -> 
      io:format("~p ~p tcp_accept (2.2) ~p~n",[?MODULE,?FUNCTION_NAME, Ref]),
      {noreply, State}
  end;

handle_cast({update_system_load}, State=#state{system_load_update_ref=undefined}) ->
  io:format("[~p:~p][{update_system_load}] ~n", [?MODULE, ?FUNCTION_NAME]),
  case State#state.socket of
    undefined -> 
      io:format("[~p:~p][{update_system_load}] Socket unavailable. ~n", [?MODULE, ?FUNCTION_NAME]),
      {noreply, State};
    Socket -> 
      io:format("[~p:~p][{update_system_load}] Socket available. ~n", [?MODULE, ?FUNCTION_NAME]),
      Ref = erlang:make_ref(),
      Result = gen_tcp:send(Socket, erlang:term_to_binary({get_system_load, Ref})),
      io:format("[~p:~p][{update_system_load}] tcp send result:~p ~n", [?MODULE, ?FUNCTION_NAME, Result]),
      {noreply, State#state{system_load_update_ref=Ref}}
  end;
handle_cast({update_system_load}, State) ->
  {noreply, State};

handle_cast({assign_task, _TaskSim={_TaskSimPid, Task}}, State) ->
  io:format("[~p:~p][assign_task]~n    Task: ~p~n", [?MODULE, ?FUNCTION_NAME, Task]),  
  gen_tcp:send(State#state.socket, erlang:term_to_binary({run_task, Task})),
  {noreply, State};

handle_cast(Request, State) ->
  io:format("handle_cast: ~p~n", [Request]),
  {noreply, State}.

handle_call({get_system_load}, _From, State) ->
  {reply, State#state.system_load, State};

handle_call(Request, _From, State) ->
  io:format("handle_call: ~p~n", [Request]),
  {noreply, State}.

handle_info({timeout, TimeRef, update_system_load}, State) ->
  io:format("[~p][loop][update_system_load] ~p~n", [?MODULE, TimeRef]),
  gen_server:cast(self(), {update_system_load}),
  TimerRef = erlang:start_timer(?INTERVAL, self(), update_system_load, [{abs, false}]),
  io:format("[~p][loop][update_system_load] TimerRef: ~p~n", [?MODULE, TimerRef]),
  {noreply, State};

handle_info({tcp, _Socket, MsgBin}, State) ->
  Msg = erlang:binary_to_term(MsgBin),
  io:format("handle_info tcp: ~p~n", [Msg]),
  tcp_recv(Msg, State);
  
handle_info(Any, State) ->
  io:format("handle_info any: ~p~n", [Any]),
  {noreply, State}.

terminate(_Reason, _Tab) -> ok.

code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

% ---

spawn() ->
  io:format("~p ~p ~n", [?MODULE, ?FUNCTION_NAME]),
  case gen_server:start_link(?MODULE, [], []) of
    {ok, Pid} -> io:format("p1 ~p p2 ~p~n",[self(),Pid]), {ok, Pid};
    {error, _Error} -> error;
    ignore -> ignore
  end.

spawn(Socket) ->
  case gen_server:start_link(?MODULE, [{socket, Socket}], []) of
    {ok, Pid} -> io:format("p1 ~p p2 ~p~n",[self(),Pid]), {ok, Pid};
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

tcp_recv({task_exec, finished, Task, ExecutionResult}, State) ->
  {noreply, State};

tcp_recv({task_exec, failed, Task}, State) ->
  {noreply, State};

tcp_recv(Msg, State) ->
  io:format("[~p:~p/~p] ~p~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Msg]),
  {noreply, State}.

% --- API

get_system_load(AgentPid) when erlang:is_pid(AgentPid) ->
  gen_server:call(AgentPid, {get_system_load});
get_system_load(_Agent=#sin_agent{pid=Pid}) ->
  get_system_load(Pid).

assign_task(_Agent=#sin_agent{pid=Pid}, Task) ->
  gen_server:cast(Pid, {assign_task, Task}).