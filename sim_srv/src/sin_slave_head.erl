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
  tasks_w8ing_4_modules :: [sin_task()]
}).

% ---
% gen_server

init(_Args) ->
  {ok, #state{}}.

handle_cast({find_master}, State) ->
  Ref = erlang:make_ref(),
  Pid = erlang:spawn_link(sin_slave_leash, init, [self(), Ref]),
  {noreply, State#state{leash_pid=Pid,leash_ref=Ref}};

handle_cast({sin_slave_leash, LeashRef, MessageFromLeash}, State) ->
  case State#state.leash_ref of
    LeashRef -> leash_cast(MessageFromLeash, State);
    _ -> {noreply, State}
  end;

handle_cast(Request, State) ->
  io:format("~p:~p: ~p~n", [?MODULE, ?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_call(Request, _From, State) ->
  io:format("~p:~p: ~p~n", [?MODULE, ?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_info(OtherInfo, State) ->
  io:format("~p:~p ~p~n",[?MODULE,?FUNCTION_NAME,OtherInfo]),
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

leash_cast({run_task, Task}, State) when erlang:is_list(Task) ->
  case sin_dep:filter_loaded(Task#sin_task.dependencies) of
    [] -> 
      RunningTasks = State#state.running_tasks ++ [run_task(Task)],
      {noreply, State#state{running_tasks=RunningTasks}};
    Modules ->
      State#state.leash_pid ! {send_to_master, {i_need_modules, Modules}},
      TasksW8 = State#state.tasks_w8ing_4_modules ++ [Task],
      {noreply, State#state{tasks_w8ing_4_modules = TasksW8}}
  end;

leash_cast(_Msg, State) ->
  {noreply, State}.

run_task(Task) ->
  #sin_running_task{}.