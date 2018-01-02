-module(sin_slave_hands).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([]).

-include("./sin_task.hrl").

-record(state,{

}).

init([{Module, Function, Args}]) ->
  {ok, #state{}}.