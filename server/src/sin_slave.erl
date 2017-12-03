-module(sin_slave).

-export([
  start/1
]).

start(SlaveConfig) when erlang:is_list(SlaveConfig) ->
  case sin_conf:is_enabled(SlaveConfig) of
    true -> run_slave_server(SlaveConfig);
    false -> ok;
    _ -> failed
  end.

run_slave_server(SlaveConfig) when erlang:is_list(SlaveConfig) ->
  tcp_connect(SlaveConfig).

tcp_connect(SlaveConfig) when erlang:is_list(SlaveConfig) ->
  MasterIp = proplists:get_value(master_ip, SlaveConfig, {127,0,0,1}),
  MasterPort = proplists:get_value(master_port, SlaveConfig, 0),
  Options = [
    binary,
    {active, false},
    {packet, 4},
    {keepalive, true},
    {port, MasterPort},
    proplists:get_value(inet, SlaveConfig, inet)
  ],
  io:format("tcp_connect options:~p~n", [Options]),
  case gen_tcp:connect(MasterIp,MasterPort,Options) of
    {ok, Socket} -> tcp_connect_ok(Socket);
    {error, _Reason} -> error
  end.

tcp_connect_ok(Socket) ->
  io:format("Connected to master!~n"),
  tcp_listen_socket(Socket).

tcp_listen_socket(Socket) ->
  receive
    {tcp, Socket, Data} -> tcp_recv(Socket, Data);
    Data -> io:format("Other message: ~p~n", [Data])
    % _ -> io:format("Other message!~n")
  end.

tcp_recv(_Socket, Data) ->
    io:format("~p ~p/~p ~p ~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Data]).    