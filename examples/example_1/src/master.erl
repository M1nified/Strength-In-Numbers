-module(master).

-export([start/0]).

start() ->
  {ok, SinPid, SinRef} = sin_srv:start([master]),
  loop_start({SinPid, SinRef}).

loop_start(Sin) ->
  loop(Sin, 0, 0, []).

loop(Sin, N, PendingCount, Results) ->
  receive
    Result = {Number, _P1, _P2} when erlang:is_number(Number) ->
      io:format("Got new result: ~p~n", [Result]),
      do_spawn(Sin, N, PendingCount-1, Results ++ [Result]);
    _ ->
      do_spawn(Sin, N, PendingCount, Results)
  after
    1 ->
      do_spawn(Sin, N, PendingCount, Results)
  end.

do_spawn(_, N, _, Results) when N > 100 ->
  Results;

do_spawn(Sin, N, PendingCount, Results) when PendingCount > 10 ->
  loop(Sin, N, PendingCount, Results);

do_spawn(Sin, N, PendingCount, Results) ->
  sin_srv:spawn(Sin, task, start, [{master, self()}, N]),
  loop(Sin, N+1, PendingCount+1, Results).