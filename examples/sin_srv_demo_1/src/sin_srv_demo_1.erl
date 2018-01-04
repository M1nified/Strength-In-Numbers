-module(sin_srv_demo_1).

%% API exports
-export([main/1]).
-export([job_1/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    start(),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

start() ->
  {ok, Link, LinkRef} = sin_srv:start([master]),
  % timer:sleep(1000),
  A = sin_srv:spawn({Link, LinkRef}, ?MODULE, job_1, [1]),
  B = sin_srv:spawn({Link, LinkRef}, ?MODULE, job_1, [2]),
  C = sin_srv:spawn({Link, LinkRef}, ?MODULE, job_1, [3]),
  D = sin_srv:spawn({Link, LinkRef}, ?MODULE, job_1, [4]),
  E = sin_srv:spawn({Link, LinkRef}, ?MODULE, job_1, [5]),
  A ! {master, self(), msg_1},
  B ! {master, self(), msg_2},
  C ! {master, self(), msg_3},
  D ! {master, self(), msg_4},
  E ! {master, self(), msg_5},
  A ! {master, self(), msg_6},
  A ! {master, self(), msg_7},
  loop().

loop() ->
  io:format("###########~n###########     [~p:~p] LOOP PID: ~p~n###########~n", [?MODULE, ?FUNCTION_NAME, self()]),
  receive
    {job_1, N, ok} ->
      io:format("###########~n###########     [~p:~p] received from job_1: ok ~p~n###########~n", [?MODULE, ?FUNCTION_NAME, N]);
    Any ->
      io:format("###########~n###########     [~p:~p] received ~p~n###########~n", [?MODULE, ?FUNCTION_NAME, Any])
  end,
  loop().

job_1(N) ->
  io:format("###########~n###########     [~p:~p(~p)] Start~n###########~n", [?MODULE, ?FUNCTION_NAME, N]),
  job_1_loop(N).

job_1_loop(N) ->
  receive
    R = {master, From, _Msg} ->
      io:format("###########~n###########     [~p:~p(~p)] Received (1): ~p~n###########~n", [?MODULE, ?FUNCTION_NAME, N, R]),
      sin_proc:master_pid(From) ! {job_1, N, ok};
    Any ->
      io:format("###########~n###########     [~p:~p(~p)] Received (2): ~p~n###########~n", [?MODULE, ?FUNCTION_NAME, N, Any])  
  end,
  job_1_loop(N).