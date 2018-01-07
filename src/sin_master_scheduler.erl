-module(sin_master_scheduler).

-author("Michał Góra").
-vsn("1.0").

-export([start/0]).

-include("./sin_debug.hrl").
-include("./sin_task.hrl").

-record(state, {
    labor_office :: pid(),
    labor_office_ref :: reference(),
    task_sims :: [{pid(), #sin_task{}}],
    task_queue :: [{pid(), #sin_task{}}],
    no_agent_counter :: number()
}).
-type state() :: #state{}.

start() ->
    spawn(fun () -> loop_start() end).

loop_start() ->
    loop(#state{task_sims=[],task_queue=[],no_agent_counter=0}).

loop(State1) ->
    State2 = loop_actions(State1),
    receive
        Any -> 
            ?DBG_INFO("[~p:~p] Received Any: ~p~n",[?MODULE,?FUNCTION_NAME,Any]),
            recv(Any, State2)
    after 
        1 ->
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
    ?DBG_INFO("[~p:~p] AgentRef: ~p~n",[?MODULE, ?FUNCTION_NAME, AgentRef]),
    sin_labor_office:assign_task(LOPid, AgentRef, Task),
    State#state{no_agent_counter=0};
assign_task_2(Task, {fail, no_agent}, State=#state{task_queue=TaskQueue,no_agent_counter=NACounter}) ->
    case NACounter rem 100000 of 0 -> ?DBG_INFO("[~p:~p] no_agent~n",[?MODULE, ?FUNCTION_NAME]); _ -> ok end,
    State#state{task_queue=TaskQueue ++ [Task], no_agent_counter=NACounter+1};
assign_task_2(Task, {fail, no_free_agent}, State=#state{task_queue=TaskQueue,no_agent_counter=NACounter}) ->
    case NACounter rem 100 of 0 -> ?DBG_INFO("[~p:~p] no_free_agent~n",[?MODULE, ?FUNCTION_NAME]); _ -> ok end,
    State#state{task_queue=TaskQueue ++ [Task], no_agent_counter=NACounter+1}.

-spec recv(tuple(), state()) -> any().
recv({labor_office, LaborOffice, LaborOfficeRef}, State) ->
    loop(State#state{labor_office=LaborOffice, labor_office_ref=LaborOfficeRef});

recv({task_sim, Task, task_msg, Msg}, State=#state{labor_office=LaborOffice}) ->
    gen_server:cast(LaborOffice, {message_to_task, Task, Msg}),
    loop(State);

recv({restart_task, Task}, State=#state{task_sims=TaskSims}) ->
    ?DBG_INFO("[~p:~p][restart_task]~n",[?MODULE,?FUNCTION_NAME]),
    case lists:filter(fun ({_, T}) -> T == Task end, TaskSims) of
        [] ->
            loop(State);
        [{TaskSim,TheTask}|_] ->
            Queue = State#state.task_queue ++ [{TaskSim, TheTask}],
            loop(State#state{task_queue = Queue})
    end;
    

recv({add_task, Module, Function, Args, From, RequestRef}, State) ->
    ?DBG_INFO("[~p:~p][add_task] ~p:~p~p~n",[?MODULE,?FUNCTION_NAME,Module,Function,Args]),
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
    ?DBG_INFO("[~p:~p] ~p ~n", [?MODULE, ?FUNCTION_NAME, Request]),
    loop(State).
