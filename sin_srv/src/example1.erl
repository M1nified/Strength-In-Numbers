-module(example1).

-export([start/0]).
-export([job_1/1]).

start() ->
  {ok, Link, LinkRef} = sin_srv:start([master]),
  A = sin_srv:spawn({Link, LinkRef}, example1, job_1, [1]),
  B = sin_srv:spawn({Link, LinkRef}, example1, job_1, [2]),
  C = sin_srv:spawn({Link, LinkRef}, example1, job_1, [3]),
  D = sin_srv:spawn({Link, LinkRef}, example1, job_1, [4]),
  E = sin_srv:spawn({Link, LinkRef}, example1, job_1, [5]),
  A ! {master, self(), msg_1},
  B ! {master, self(), msg_1},
  C ! {master, self(), msg_1},
  D ! {master, self(), msg_1},
  E ! {master, self(), msg_1},
  A ! {master, self(), msg_1},
  A ! {master, self(), msg_1},
  loop().

loop() ->
  receive
    {job_1, N, ok} ->
      io:format("     [~p:~p] received from job_1: ok ~p~n", [?MODULE, ?FUNCTION_NAME, N]);
    Any ->
      io:format("     [~p:~p] received ~p~n", [?MODULE, ?FUNCTION_NAME, Any])
  end.

job_1(N) ->
  io:format("     [~p:~p(~p)] Start~n", [?MODULE, ?FUNCTION_NAME, N]),
  job_1_loop(N).

job_1_loop(N) ->
  receive
    R = {master, From, _Msg} ->
      io:format("     [~p:~p(~p)] Received: ~p~n", [?MODULE, ?FUNCTION_NAME, N, R]),
      sin_proc:master_pid(From) ! {job_1, N, ok};
    Any ->
      io:format("     [~p:~p(~p)] Received: ~p~n", [?MODULE, ?FUNCTION_NAME, N, Any])  
  end,
  job_1_loop(N).

