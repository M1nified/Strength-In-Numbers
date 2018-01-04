-module(sin_slave_head).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([rise/0, rise/1]).
-export([find_master/1]).

-include("./sin_task.hrl").

-record(state,{
  leash_pid :: pid(),
  leash_ref :: reference(),
  running_tasks :: [sin_running_task()],
  ordered_modules :: [module()],
  tasks_w8ing_4_modules :: [sin_task()]
}).

% ---
% gen_server

init(_Args) ->
  sin_proc:add_client(self()),
  {ok, #state{running_tasks=[],tasks_w8ing_4_modules=[],ordered_modules=[]}}.

handle_cast({find_master}, State) ->
  Ref = erlang:make_ref(),
  Pid = erlang:spawn_link(sin_slave_leash, init, [self(), Ref]),
  {noreply, State#state{leash_pid=Pid,leash_ref=Ref}};

handle_cast({run_task, Task}, State) ->
  handle_cast_2({run_task, Task}, State);

handle_cast({sin_slave_leash, LeashRef, MessageFromLeash}, State) ->
  io:format("[~p:~p][sin_slave_leash]~n    LeashRef: ~p~n    Message: ~p~n", [?MODULE, ?FUNCTION_NAME, LeashRef, MessageFromLeash]),
  case State#state.leash_ref of
    LeashRef -> handle_cast_2(MessageFromLeash, State);
    _ -> {noreply, State}
  end;

handle_cast({execution, ExecutionRef, started}, State=#state{running_tasks=RunningTasks}) ->
  io:format("[~p:~p][execution][started] Ref: ~p~n", [?MODULE, ?FUNCTION_NAME, ExecutionRef]),
  case lists:filter(fun (#sin_running_task{execution_ref=Ref}) -> Ref == ExecutionRef end, RunningTasks) of
    [] ->
      {noreply, State};
    [#sin_running_task{task=Task}|_] ->
      State#state.leash_pid ! {send_to_master, {task_exec, started, Task}},
      {noreply, State}
  end;

handle_cast({execution, ExecutionRef, finished, ExecutionResult}, State) ->
  io:format("[~p:~p][execution][finished] Ref: ~p~n", [?MODULE, ?FUNCTION_NAME, ExecutionRef]),
  io:format("[~p:~p][execution][finished] Result: ~p~n", [?MODULE, ?FUNCTION_NAME, ExecutionResult]),
  case pop_from_running_tasks_by_ref(ExecutionRef, State) of
    {undefined, State2} ->
      {noreply, State2};
    {RunningTask, State2} ->
      State2#state.leash_pid ! {send_to_master, {task_exec, finished, RunningTask#sin_running_task.task, ExecutionResult}},
      {noreply, State2}
  end;

handle_cast({execution, ExecutionRef, failed}, State) ->
  io:format("[~p:~p][execution][failed] Ref: ~p~n", [?MODULE, ?FUNCTION_NAME, ExecutionRef]),
  case pop_from_running_tasks_by_ref(ExecutionRef, State) of
    {undefined, State2} ->
      {noreply, State2};
    {RunningTask, State2} ->
      State2#state.leash_pid ! {send_to_master, {task_exec, failed, RunningTask#sin_running_task.task}},
      {noreply, State2}
  end;

handle_cast(Request, State) ->
  io:format("[~p:~p] ~p~n", [?MODULE, ?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_call(Request, _From, State) ->
  io:format("[~p:~p] ~p~n", [?MODULE, ?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_info({sin_proc, captured_message, to_master, MasterProc, Msg}, State) ->
  State#state.leash_pid ! {send_to_master, {message_to_proc, MasterProc, Msg}},
  {noreply, State};

handle_info(OtherInfo, State) ->
  io:format("[~p:~p] ~p~n",[?MODULE,?FUNCTION_NAME,OtherInfo]),
  {noreply, State}.
  
terminate(_Reason, _Tab) -> ok.

code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

% ---

rise() ->
  rise([]).

rise(Options) ->
  case gen_server:start_link(?MODULE, Options, []) of
    {ok, Pid} -> {ok, Pid};
    {error, _Error} -> error;
    ignore -> ignore
  end.

find_master(SlaveHead) ->
  gen_server:cast(SlaveHead, {find_master}).

% ---

handle_cast_2({run_task, Task}, State) ->
  io:format("[~p:~p][run_task]~n    Task: ~p~n", [?MODULE, ?FUNCTION_NAME, Task]),
  case list_missing_modules(Task) of
    [] -> 
      io:format("[~p:~p][run_task] Got all modules. ~n", [?MODULE, ?FUNCTION_NAME]),
      RunningTasks = State#state.running_tasks ++ [run_task(Task)],
      {noreply, State#state{running_tasks=RunningTasks}};
    ModulesAndProblems ->
      io:format("[~p:~p][run_task] Requires additional modules:~n    ~p~n", [?MODULE, ?FUNCTION_NAME, ModulesAndProblems]),
      TasksW8 = State#state.tasks_w8ing_4_modules ++ [Task],
      case lists:filtermap(fun ({Module, _Problem}) -> case lists:member(Module, State#state.ordered_modules) of true -> false; _ -> {true, Module} end end, ModulesAndProblems) of
        [] ->
          io:format("[~p:~p][run_task] Modules already ordered.~n", [?MODULE, ?FUNCTION_NAME]),
          {noreply, State#state{tasks_w8ing_4_modules=TasksW8}};
        ModulesToOrder ->
          io:format("[~p:~p][run_task] Will order modules: ~p~n", [?MODULE, ?FUNCTION_NAME, ModulesToOrder]),
          State#state.leash_pid ! {send_to_master, {get_modules, ModulesToOrder}},
          OrderedModules = State#state.ordered_modules ++ ModulesToOrder,
          {noreply, State#state{tasks_w8ing_4_modules=TasksW8, ordered_modules=OrderedModules}}
      end
  end;

handle_cast_2({update_modules, Modules}, State=#state{ordered_modules=OrderedModules}) ->
  LoadedModules = lists:map(fun (Tup={Module, _Binary, _}) -> sin_code:load_module(Tup), Module end, Modules),
  OrderedModules2 = lists:filter(fun (Module) -> not lists:member(Module, LoadedModules) end, OrderedModules),
  lists:foreach(fun (Task) -> gen_server:cast(self(), {run_task, Task}) end, State#state.tasks_w8ing_4_modules),
  {noreply, State#state{ordered_modules=OrderedModules2,tasks_w8ing_4_modules=[]}};

handle_cast_2({message_to_task, Task, Msg}, State=#state{running_tasks=RunningTasks}) ->
  io:format("[~p:~p][message_to_task]~n    Msg: ~p~n", [?MODULE, ?FUNCTION_NAME, Msg]),
  case lists:filter(fun (#sin_running_task{task=#sin_task{ref=TaskRef}}) -> TaskRef == Task#sin_task.ref end, RunningTasks) of
    [RTask|_] ->
      RTask#sin_running_task.pid ! {message_to_task, Msg},
      {noreply, State};
    _ ->
      {noreply, State}
  end;

handle_cast_2(_Msg, State) ->
  {noreply, State}.

run_task(Task) ->
  {ok, ExecutionRef, Pid} = sin_slave_executor:execute(self(), Task),
  #sin_running_task{pid=Pid,task=Task,execution_ref=ExecutionRef}.

pop_from_running_tasks_by_ref(ExecutionRef, State=#state{running_tasks=RunningTasks}) ->
  io:format("[~p:~p] ~p~n", [?MODULE, ?FUNCTION_NAME, RunningTasks]),
  case lists:filter(fun (#sin_running_task{execution_ref=Ref}) -> Ref == ExecutionRef end, RunningTasks) of
    [RTask | _] ->
      io:format("A~n"),
      RTasks = lists:delete(RTask, RunningTasks),
      {RTask, State#state{running_tasks=RTasks}};
    _ ->
      io:format("B~n"),
      {undefined, State}
  end.

% ---

list_missing_modules(_Task=#sin_task{dependencies=Deps}) ->
  case code:ensure_modules_loaded(Deps) of
    ok -> 
      [];
    {error, Modules} ->
      Modules
  end.
