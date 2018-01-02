-module(sin_slave_executor).

-export([execute/2, execute/3]).

-include("./sin_task.hrl").

execute(SlaveHead, Task) ->
  Ref = make_ref(),
  execute(Ref, SlaveHead, Task).

execute(ExecutionRef, SlaveHead, Task) ->
  Pid = erlang:spawn(fun () -> execute_2(ExecutionRef, SlaveHead, Task) end),
  {ok, ExecutionRef, Pid}.

execute_2(ExecutionRef, SlaveHead, Task) ->
  Ref = erlang:make_ref(),
  Self = erlang:self(),
  Pid = erlang:spawn(fun () -> execute_3(Self, Ref, Task) end),
  MonitorRef = erlang:monitor(process, Pid),
  receive
    {Ref, execution_result, ExecutionResult} ->
      gen_server:cast(SlaveHead, {execution, ExecutionRef, finished, ExecutionResult});
    {"DOWN", MonitorRef, _, _, _} ->
      gen_server:cast(SlaveHead, {execution, ExecutionRef, failed})
  end.
  
execute_3(Self, Ref, Task) ->
  #sin_task{spawn_3={Module, Function, Arguments}} = Task,
  ExecutionResult = erlang:apply(Module, Function, Arguments),
  Self ! {Ref, execution_result, ExecutionResult}.
