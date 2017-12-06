-module(sin_master).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("../headers/types.hrl").

init(_Args) ->
  sin_master_networking:start([{slaves_port, 3456}]),
  {ok, #sin_master_state{}}.

handle_call(Request, _From, State) ->
  io:format("Request: ~p~n", [Request]),
  {noreply, State}.
  
handle_cast(Request, State) ->
  io:format("Request: ~p~n", [Request]),
  {noreply, State}.

handle_info(_Info, State) ->
  io:format("State is: ~p~n", [State]),
  {noreply, State}.