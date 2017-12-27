-module(sin_master_task_sim).

-export([start/1]).

-record(state, {
    task :: term()
}).

start(Task) ->
    spawn(fun () -> loop_start(Task) end).

loop_start(Task) ->
    loop(#state{
        task=Task
    }).

loop(State) ->
    receive
        _ -> loop(State)
            
    after
        100 -> loop(State)
            
    end.
