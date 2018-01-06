-module(task).

-export([start/2]).

start({master, From}, N) ->
  start(sin_srv:master_pid(From), N);
start(From, N) ->
  io:format("N=~p~n",[N]),
  timer:sleep(5000),
  From ! {N, math:pow(2,N), math:pow(N,2)}.