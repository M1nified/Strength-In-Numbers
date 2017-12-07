-module(sin_labor_office).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-behaviour(sin_hunter).
-export([handle_accept/1]).

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
  Hunter = sin_hunter:make(HunterConfig, ?MODULE),
  {ok, #state{hunter=Hunter}}.

handle_cast({tcp_listen}, State) ->
  io:format("~p ~p tcp_listen ~n", [?MODULE, ?FUNCTION_NAME]),
  sin_hunter:tcp_listen(State#state.hunter),
  {noreply, State};
  
handle_cast({tcp_accept, ListenSocket}, State)->
  io:format("~p ~p tcp_accept (1) ~n", [?MODULE, ?FUNCTION_NAME]),
  case sin_agent:spawn() of
    {ok, Pid} ->
      io:format("~p ~p tcp_accept (2.1) ~n", [?MODULE, ?FUNCTION_NAME]),
      Ref = erlang:make_ref(),
      gen_server:cast(Pid, {tcp_accept, ListenSocket, Ref}),
      {noreply, State};
    _ -> 
      io:format("~p ~p tcp_accept (2.2) ~n", [?MODULE, ?FUNCTION_NAME]),
      {noreply, State}
  end;

handle_cast(Request, State) ->
  io:format("handle_cast: ~p~n", [Request]),
  {noreply, State}.

handle_call(Request, _From, State) ->
  io:format("handle_call: ~p~n", [Request]),
  {noreply, State}.

handle_info({capture, HunterRef, {socket, Socket}}, State) ->
  case sin_hunter:get_ref(State#state.hunter) of
    HunterRef -> {noreply, add_agent(State, Socket)};
    _ -> {noreply, State}
  end;

handle_info(OtherInfo, State) ->
  io:format("~p ~p ~p~n",[?MODULE,?FUNCTION_NAME,OtherInfo]),
  {noreply, State}.

terminate(_Reason, _Tab) -> ok.

code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

% ---
% sin_hunter

handle_accept(ListenSocket) ->
  gen_server:cast(self(), {tcp_accept, ListenSocket}).

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