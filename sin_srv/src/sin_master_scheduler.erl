-module(sin_master_scheduler).

-export([start/0]).

-include("./sin_task.hrl").

-record(state, {
    labor_office :: pid(),
    labor_office_ref :: reference(),
    task_sims :: [{pid(), #sin_task{}}],
    task_queue :: [{pid(), #sin_task{}}]
}).
-type state() :: #state{}.

start() ->
    spawn(fun () -> loop_start() end).

loop_start() ->
    loop(#state{}).

loop(State) ->
    NewState = loop_actions(State),
    receive
        Any -> recv(Any, NewState)
    after 
        0 ->
            loop(NewState)
    end.

-spec loop_actions(state()) -> state().
loop_actions(State) ->
    State2 = assign_task(State),
    State2.

-spec assign_task(state()) -> state().
assign_task(State=#state{task_queue=[]}) ->
    State;
assign_task(State=#state{task_queue=[Task]}) ->
    Response = gen_server:call(State#state.labor_office, {get_best_slave}),
    assign_task_2(Task, Response, State#state{task_queue=[]});
assign_task(State=#state{task_queue=[Task|Queue]}) ->
    Response = gen_server:call(State#state.labor_office, {get_best_slave}),
    assign_task_2(Task, Response, State#state{task_queue=Queue}).

-spec assign_task_2(any(), any(), state()) -> state().
assign_task_2(TaskResponse, _BestSlave={ok, Agent}, State) ->
    State.

recv({labor_office, LaborOffice, LaborOfficeRef}, State) ->
    loop(State#state{labor_office=LaborOffice, labor_office_ref=LaborOfficeRef});

recv({add_task, Module, Function, Args, From, RequestRef}, State) ->
    Task = #sin_task{
        spawn_3={Module, Function, Args},
        dependencies=sin_dep:needs(Module)
    },
    TaskSim = sin_master_task_sim:start(Task),
    From ! {task_sim, RequestRef, TaskSim},
    State2 = State#state{
        task_sims=State#state.task_sims ++ [{TaskSim, Task}],
        task_queue=State#state.task_queue ++ [{TaskSim, Task}]
    },
    loop(State2);

recv(Request, State) ->
    io:format("~p ~p ~p ~n", [?MODULE, ?FUNCTION_NAME, Request]),
    loop(State).
