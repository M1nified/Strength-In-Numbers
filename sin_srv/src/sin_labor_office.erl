-module(sin_labor_office).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-behaviour(sin_hunter).
-export([handle_accept/1]).

-export([
  open/1,
  assign_task/3
]).

-include("./sin_agent.hrl").
-include("./sin_system_load.hrl").
-include("./sin_task.hrl").

-record(state,{
  pid :: pid(),
  ref :: reference(),
  hunter :: any(),
  acceptors :: [{reference(), sin_agent()}],
  agents :: [{reference(), sin_agent()}],
  scheduler :: pid(),
  assigned_tasks :: [{sin_task(), sin_agent()}],
  pending_messages :: [{sin_task(), any()}]
}).

% ---
% gen_server

init(HunterConfig) ->
  erlang:register(?MODULE, erlang:self()),
  Hunter = sin_hunter:make(HunterConfig, ?MODULE),
  Scheduler = sin_master_scheduler:start(),
  Scheduler ! {labor_office, self(), undefined},
  {ok, #state{
    hunter=Hunter, 
    acceptors=[],
    agents=[],
    scheduler=Scheduler,
    assigned_tasks=[],
    pending_messages=[]
  }}.

handle_cast({tcp_listen}, State) ->
  io:format("~p ~p tcp_listen ~n", [?MODULE, ?FUNCTION_NAME]),
  sin_hunter:tcp_listen(State#state.hunter),
  {noreply, State};
  
handle_cast({tcp_accept, ListenSocket}, State)->
  io:format("~p ~p tcp_accept (1) ~n", [?MODULE, ?FUNCTION_NAME]),
  case sin_agent:start() of
    {ok, Agent=#sin_agent{pid=Pid}} ->
      io:format("~p ~p tcp_accept (2.1) ~n", [?MODULE, ?FUNCTION_NAME]),
      Ref = erlang:make_ref(),
      gen_server:cast(Pid, {tcp_accept, ListenSocket, self(), Ref}),
      {noreply, State#state{
        acceptors = State#state.acceptors ++ [{Ref, Agent}]
      }};
    _ -> 
      io:format("~p ~p tcp_accept (2.2) ~n", [?MODULE, ?FUNCTION_NAME]),
      {noreply, State}
  end;

handle_cast({tcp_accepted, Ref}, State) when erlang:is_reference(Ref) ->
  io:format("~p ~p tcp_accepted ~n", [?MODULE, ?FUNCTION_NAME]),
  case proplists:get_value(Ref, State#state.acceptors) of
    undefined -> {noreply, State};
    Agent ->  % sin_agent()
      Acceptors = lists:filter(fun ({Ref2, _}) -> Ref =/= Ref2 end, State#state.acceptors),
      Agents = State#state.agents ++ [{Ref, Agent}],
      {noreply, State#state{
        acceptors = Acceptors,
        agents = Agents
      }}
  end;

handle_cast({info, show_state}, State) ->
  io:format("~p state is:~n~p~n",[?MODULE, State]),
  {noreply, State};

handle_cast({resend_messages_for_task, Task=#sin_task{ref=TaskRef}}, State=#state{pending_messages=PendingMessages}) ->
  io:format("[~p:~p][resend_messages_for_task]~n", [?MODULE, ?FUNCTION_NAME]),
  io:format("[~p:~p][resend_messages_for_task] All PendingMessages: ~p~n", [?MODULE, ?FUNCTION_NAME, PendingMessages]),
  Queue = lists:filtermap(fun ({#sin_task{ref=Ref},Msg}) -> case Ref==TaskRef of true -> {true, Msg}; _ -> false end end, PendingMessages),
  io:format("[~p:~p][resend_messages_for_task] Queue: ~p~n", [?MODULE, ?FUNCTION_NAME, Queue]),
  lists:foreach(fun (Msg) -> gen_server:cast(self(), {message_to_task, Task, Msg}) end, Queue),
  PendingMessages2 = lists:filter(fun ({T, _}) -> T /= Task end, PendingMessages),
  {noreply, State#state{pending_messages=PendingMessages2}};

handle_cast(Request={message_to_task, Task=#sin_task{ref=Ref}, Message}, State=#state{assigned_tasks=AssignedTasks}) ->
  io:format("[~p:~p][message_to_task]~n    Task: ~p~n    Message: ~p~n", [?MODULE, ?FUNCTION_NAME, Task, Message]),
  io:format("[~p:~p][message_to_task]~n    AssignedTasks: ~p~n", [?MODULE, ?FUNCTION_NAME, AssignedTasks]),
  case lists:filter(fun ({#sin_task{ref=R}, _}) -> R == Ref end, AssignedTasks) of
    [] ->
      {noreply, State#state{pending_messages=State#state.pending_messages++[{Task, Message}]}};
    [{_,Agent}|_] -> 
      sin_agent:cast(Agent, Request),
      {noreply, State}
  end;

handle_cast({assign_task, AgentRef, Task}, State) when erlang:is_reference(AgentRef) ->
  io:format("[~p:~p][assign_task] ~n    Agent: ~p~n    Task: ~p~n", [?MODULE, ?FUNCTION_NAME, AgentRef, Task]),
  case proplists:get_value(AgentRef, State#state.agents) of
    undefined ->
      {noreply, State};
    Agent ->
      sin_agent:assign_task(Agent, Task),
      {noreply, State#state{assigned_tasks=State#state.assigned_tasks++[{Task, Agent}]}}
  end;

handle_cast(Request, State) ->
  io:format("handle_cast: ~p~n", [Request]),
  {noreply, State}.

handle_call({add_task, Module, Function, Args}, _From, State) ->
  io:format("[~p:~p][add_task] ~p:~p~p~n",[?MODULE, ?FUNCTION_NAME, Module, Function,Args]),
  Ref = erlang:make_ref(),
  State#state.scheduler ! {add_task, Module, Function, Args, self(), Ref},
  io:format("[~p:~p][add_task] ~p:~p~p SENT TO SCHEDULER!!!~n",[?MODULE, ?FUNCTION_NAME, Module, Function,Args]),  
  receive
    {task_sim, Ref, TaskSimPid} ->
      {reply, {ok, TaskSimPid}, State}
  after
    5000 ->
      {reply, {error, timeout}, State}
  end;

handle_call({get_best_slave}, _From, State=#state{agents=[_|_]}) ->
  Loads = lists:map(fun ({Ref, #sin_agent{pid=Pid}}) -> #sin_system_load{cpu_avg_1=Load} = sin_agent:get_system_load(Pid), {Ref, Load} end, State#state.agents),
  [{Best, BestLoad} | _Rest] = lists:sort(fun ({_ARef, ALoad},{_BRef, BLoad}) -> ALoad =< BLoad end, Loads),
  case BestLoad > 200 of
    true ->
      {reply, {fail, no_free_agent}, State};
    _ ->
      {reply, {ok, Best}, State}
  end;
handle_call({get_best_slave}, _From, State) ->
  {reply, {fail, no_agent}, State};

handle_call(Request, _From, State) ->
  io:format("handle_call: ~p~n", [Request]),
  {noreply, State}.

handle_info(OtherInfo, State) ->
  io:format("~p ~p ~p~n",[?MODULE,?FUNCTION_NAME,OtherInfo]),
  {noreply, State}.

terminate(_Reason, _Tab) -> ok.

code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

% ---
% sin_hunter

handle_accept(ListenSocket) ->
  [gen_server:cast(self(), {tcp_accept, ListenSocket}) || _ <- lists:seq(1,5)].

% --- API

open(HunterConfig) ->
  case gen_server:start_link(?MODULE, HunterConfig, []) of
    {ok, Pid} -> {ok, Pid};
    {error, _Error} -> error;
    ignore -> ignore
  end.

assign_task(LaborOfficePid, AgentRef, Task) when erlang:is_reference(AgentRef) ->
  gen_server:cast(LaborOfficePid, {assign_task, AgentRef, Task}).
