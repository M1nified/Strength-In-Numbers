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
    loop(#state{task_sims=[],task_queue=[]}).

loop(State1) ->
    State2 = loop_actions(State1),
    receive
        Any -> recv(Any, State2)
    after 
        0 ->
            loop(State2)
    end.

-spec loop_actions(state()) -> state().
loop_actions(State) ->
    State2 = assign_task(State),
    State2.

-spec assign_task(state()) -> state().
assign_task(State=#state{task_queue=[]}) ->
    State;
assign_task(State=#state{task_queue=[Task|Queue]}) ->
    Response = gen_server:call(State#state.labor_office, {get_best_slave}),
    assign_task_2(Task, Response, State#state{task_queue=Queue});
assign_task(State) ->
    State.

-spec assign_task_2(any(), any(), state()) -> state().
assign_task_2({_,Task}, _BestSlave={ok, AgentRef}, State=#state{labor_office=LOPid}) ->
    io:format("[~p:~p] AgentRef: ~p~n",[?MODULE, ?FUNCTION_NAME, AgentRef]),
    sin_labor_office:assign_task(LOPid, AgentRef, Task),
    State;
assign_task_2(Task, {fail, no_agent}, State=#state{task_queue=TaskQueue}) ->
    io:format("[~p:~p] no_agent~n",[?MODULE, ?FUNCTION_NAME]),
    State#state{task_queue=TaskQueue ++ [Task]}.

-spec recv(tuple(), state()) -> any().
recv({labor_office, LaborOffice, LaborOfficeRef}, State) ->
    loop(State#state{labor_office=LaborOffice, labor_office_ref=LaborOfficeRef});

recv({task_sim, Task, task_msg, Msg}, State=#state{labor_office=LaborOffice}) ->
    gen_server:cast(LaborOffice, {message_to_task, Task, Msg}),
    loop(State);

recv({add_task, Module, Function, Args, From, RequestRef}, State) ->
    Task = #sin_task{
        ref=erlang:make_ref(),
        spawn_3={Module, Function, Args},
        dependencies=sin_dep:fun_needs_modules(Module, Function, Args)
    },
    TaskSim = sin_master_task_sim:start(Task, self()),
    From ! {task_sim, RequestRef, TaskSim},
    State2 = State#state{
        task_sims=State#state.task_sims ++ [{TaskSim, Task}],
        task_queue=State#state.task_queue ++ [{TaskSim, Task}]
    },
    loop(State2);

recv(Request, State) ->
    io:format("~p ~p ~p ~n", [?MODULE, ?FUNCTION_NAME, Request]),
    loop(State).
