-module(sin_proc).

-behaviour(gen_server).
-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([
  spawn/1,
  spawn/3
]).

-record(state,{
  pid :: pid(),
  ref :: reference()
}).
-type state() :: #state{}.

spawn(Callback) when erlang:is_function(Callback) ->
  case gen_server:start(sin_proc, [Callback], []) of
    {ok, Pid} -> Pid;
    {error, _Error} -> error;
    ignore -> ignore
  end;

spawn(_Callback) ->
  callback_is_not_a_function.

spawn(Module, Function, Args) ->
  case gen_server:start(sin_proc, [Module, Function, Args],[]) of
    {ok, Pid} -> Pid;
    {error, _Error} -> error;
    ignore -> ignore
  end.
  % erlang:spawn(fun () -> start(Module, Function, Args) end).

% start(Module, Function, Args) ->

% ---
init([Callback]) when erlang:is_function(Callback) ->
  Ref = erlang:make_ref(),
  Pid =  erlang:spawn(fun () -> io:format("Spawned~n"), Callback() end),
  {ok, #state{pid=Pid,ref=Ref}};

init([Module, Function, Args]) ->
  Ref = erlang:make_ref(),
  Pid = erlang:spawn(fun () -> io:format("Spawned~n") end),
  {ok, #state{pid=Pid,ref=Ref}}.

handle_cast(Request, State) ->
  io:format("handle_cast: ~p~n", [Request]),
  {noreply, State}.

handle_call(Request, _From, State) ->
  io:format("handle_call: ~p~n", [Request]),
  {noreply, State}.

% handles all direct messages
handle_info(Request, State) -> 
  io:format("handle_info: ~p~n", [Request]),
  {noreply, State}.

terminate(_Reason, _Tab) -> ok.

code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.
