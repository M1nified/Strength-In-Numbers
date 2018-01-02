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

-record(state,{
  pid :: pid(),
  ref :: reference(),
  hunter :: any(),
  acceptors :: [{reference(), sin_agent()}],
  agents :: [{reference(), sin_agent()}],
  scheduler :: pid()
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
    scheduler=Scheduler
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

handle_cast({assign_task, AgentRef, Task}, State) when erlang:is_reference(AgentRef) ->
  io:format("[~p:~p][assign_task] ~n    Agent: ~p~n    Task: ~p~n", [?MODULE, ?FUNCTION_NAME, AgentRef, Task]),
  case proplists:get_value(AgentRef, State#state.agents) of
    undefined ->
      {noreply, State};
    Agent ->
      sin_agent:assign_task(Agent, Task),
      {noreply, State}
  end;

handle_cast(Request, State) ->
  io:format("handle_cast: ~p~n", [Request]),
  {noreply, State}.

handle_call({add_task, Module, Function, Args}, _From, State) ->
  Ref = erlang:make_ref(),
  State#state.scheduler ! {add_task, Module, Function, Args, self(), Ref},
  receive
    {task_sim, Ref, TaskSimPid} ->
      {reply, {ok, TaskSimPid}, State}
  after
    5000 ->
      {reply, {error, timeout}, State}
  end;

handle_call({get_best_slave}, _From, State=#state{agents=[_|_]}) ->
  Loads = lists:map(fun ({Ref, #sin_agent{pid=Pid}}) -> #sin_system_load{cpu_avg_1=Load} = sin_agent:get_system_load(Pid), {Ref, Load} end, State#state.agents),
  io:format("[~p:~p][get_best_slave] Loads: ~p~n",[?MODULE, ?FUNCTION_NAME, Loads]),
  [{Best, _} | _Rest] = lists:sort(fun ({_ARef, ALoad},{_BRef, BLoad}) -> ALoad =< BLoad end, Loads),
  {reply, {ok, Best}, State};
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
