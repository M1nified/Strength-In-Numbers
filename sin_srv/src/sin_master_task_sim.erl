-module(sin_master_task_sim).

-export([start/2]).

-record(state, {
    task :: term(),
    scheduler :: pid()
}).

start(Task, Scheduler) ->
    spawn(fun () -> loop_start(Task, Scheduler) end).

loop_start(Task, Scheduler) ->
    loop(#state{
        task=Task,
        scheduler=Scheduler
    }).

loop(State=#state{task=Task, scheduler=Scheduler}) ->
    receive
        Msg -> 
            Scheduler ! {task_sim, Task, task_msg, Msg},
            loop(State)
    end.
