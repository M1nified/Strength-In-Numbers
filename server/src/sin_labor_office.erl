-module(sin_labor_office).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-behaviour(sin_hunter).
-export([handle_capture/2]).

-export([
  open/1
]).

-record(state,{
  pid :: pid(),
  ref :: reference(),
  hunter :: any()
}).

% ---
% gen_server

init(HunterConfig) ->
  erlang:register(sin_labor_office, erlang:self()),
  Hunter = sin_hunter:spawn(HunterConfig, sin_labor_office),
  {ok, #state{hunter=Hunter}}.
  
handle_cast(Request, State) ->
  io:format("handle_cast: ~p~n", [Request]),
  {noreply, State}.

handle_call(Request, _From, State) ->
  io:format("handle_call: ~p~n", [Request]),
  {noreply, State}.

handle_info({capture, HunterRef, {socket, Socket}}, State) ->
  case sin_hunter:get_ref(State#state.hunter) of
    HunterRef -> add_agent(State, Socket);
    _ -> {noreply, State}
  end.

terminate(_Reason, _Tab) -> ok.

code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

% ---
% sin_hunter

handle_capture(Hunter, Socket) ->
  {report, {capture, sin_hunter:get_ref(Hunter), {socket, Socket}}}.

% ---

open(HunterConfig) ->
  case gen_server:start_link(sin_labor_office, HunterConfig, []) of
    {ok, Pid} -> Pid;
    {error, _Error} -> error;
    ignore -> ignore
  end.

% ---

add_agent(State, Socket) ->
  io:format("add_agent~n"),
  sin_agent:spawn(Socket),
  State.