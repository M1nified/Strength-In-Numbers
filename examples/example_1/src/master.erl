-module(master).

-export([start/0]).

start() ->
  {ok, SinPid, SinRef} = sin_srv:start([master, [{port, 3456}]]).