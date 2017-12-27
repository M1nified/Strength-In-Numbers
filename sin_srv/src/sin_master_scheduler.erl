-module(sin_master_scheduler).

-export([start/0]).

-include("./sin_task.hrl").

-record(state, {
    labor_office :: pid(),
    labor_office_ref :: reference(),
    task_sims :: [{pid(), term()}]
}).

start() ->
    spawn(fun () -> loop_start() end).

loop_start() ->
    loop(#state{}).

loop(State) ->
    receive
        Any -> recv(Any, State)
    after 0 ->
        loop(State)
    end.

recv({labor_office, LaborOffice, LaborOfficeRef}, State) ->
    loop(State#state{labor_office=LaborOffice, labor_office_ref=LaborOfficeRef});

recv({add_task, Module, Function, Args, From, RequestRef}, State) ->
    Task = #sin_task{
        spawn_3={Module, Function, Args},
        dependencies=sin_dep:needs(Module)
    },
    TaskSim = sin_master_task_sim:start(Task),
    From ! {task_sim, RequestRef, TaskSim},
    loop(State);

recv(Request, State) ->
    io:format("~p ~p ~p ~n", [?MODULE, ?FUNCTION_NAME, Request]),
    loop(State).
