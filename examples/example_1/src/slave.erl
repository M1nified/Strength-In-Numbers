-module(slave).

-export([start/0]).

start() ->
  sin_srv:start([slave, [{port, 3456}]]).