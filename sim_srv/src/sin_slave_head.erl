-module(sin_slave_head).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([rise/0, rise/1]).
-export([find_master/1]).

-record(state,{
  leash_pid :: pid(),
  leash_ref :: reference()
}).

% ---
% gen_server

init(_Args) ->
  {ok, #state{}}.

handle_cast({find_master}, State) ->
  Ref = erlang:make_ref(),
  Pid = erlang:spawn_link(sin_slave_leash, init, [self(), Ref]),
  {noreply, State#state{leash_pid=Pid,leash_ref=Ref}};

handle_cast(Request, State) ->
  io:format("~p:~p: ~p~n", [?MODULE, ?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_call(Request, _From, State) ->
  io:format("~p:~p: ~p~n", [?MODULE, ?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_info(OtherInfo, State) ->
  io:format("~p:~p ~p~n",[?MODULE,?FUNCTION_NAME,OtherInfo]),
  {noreply, State}.
  
terminate(_Reason, _Tab) -> ok.

code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

% ---

rise() ->
  rise([]).

rise(Options) ->
  case gen_server:start_link(?MODULE, Options, []) of
    {ok, Pid} -> {ok, Pid};
    {error, _Error} -> error;
    ignore -> ignore
  end.

find_master(SlaveHead) ->
  gen_server:cast(SlaveHead, {find_master}).