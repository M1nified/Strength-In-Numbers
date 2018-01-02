-module(sin_link).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([open/0]).

-record(state,{
  parent :: pid(),
  ref :: reference(),
  service :: pid()
}).

init(Args) ->
  {ok, #state{
    parent=proplists:get_value(parent, Args),
    ref=proplists:get_value(ref, Args)
    }}.

handle_cast({LinkRef, {service, ServicePid}}, State) ->
  io:format("~p ~p ~p ~n", [?MODULE, ?FUNCTION_NAME, {service, ServicePid}]),
  case State#state.ref of 
    LinkRef ->
      NewState = State#state{service=ServicePid};
    _ ->
      NewState = State
  end,
  {noreply, NewState};

handle_cast(Request, State) ->
  io:format("~p ~p ~p ~n", [?MODULE, ?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_call({LinkRef, {spawn, Module, Function, Args}}, _From, State) ->
  io:format("~p ~p ~p ~n", [?MODULE, ?FUNCTION_NAME, {spawn, Module, Function, Args}]),
  case State#state.ref of
    LinkRef ->
      {ok, Pid} = gen_server:call(State#state.service, {add_task, Module, Function, Args}), 
      {reply, {ok, Pid}, State};
    _ ->
      {reply, {error}, State}
  end;

handle_call(Request, _From, State) ->
  io:format("~p ~p ~p ~n", [?MODULE, ?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_info({Fifo, {data, Data}}, State) when erlang:is_port(Fifo) ->
  io:format("~p ~p fifo ~p ~n", [?MODULE, ?FUNCTION_NAME, Data]),
  {noreply, State};

handle_info(Any, State) ->
  io:format("~p ~p ~p ~n", [?MODULE, ?FUNCTION_NAME, Any]),
  {noreply, State}.

terminate(_Reason, _Tab) -> ok.
  
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

% ---

open() ->
  Ref = erlang:make_ref(),
  case gen_server:start_link(?MODULE, [{parent, self()},{ref, Ref}], []) of
    {ok, Pid} -> {ok, Pid, Ref};
    Else -> Else
  end.
    