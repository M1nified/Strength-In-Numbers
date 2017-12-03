-module(sin_master).
-export([
  start/1
]).

start(MasterConfig) when erlang:is_list(MasterConfig) ->
  case sin_conf:is_enabled(MasterConfig) of
    true -> run_master_server(MasterConfig);
    false -> ok;
    _ -> failed
  end.

run_master_server(MasterConfig) ->
  tcp_listen(MasterConfig).

tcp_listen(MasterConfig) ->
  Options = [
    binary,
    {active, false},
    {keepalive, true},
    {packet, 4},
    {port, proplists:get_value(slaves_port, MasterConfig, 0)},
    {ip, proplists:get_value(interface_ip, MasterConfig, "127.0.0.1")},
    proplists:get_value(inet, MasterConfig, inet)
  ],
  case gen_tcp:listen(0,Options) of
    {ok, ListenSocket} -> tcp_listen_ok(ListenSocket);
    {error, _Reason} -> error
  end.

tcp_listen_ok(ListenSocket) ->
  case inet:port(ListenSocket) of
    {ok, Port} -> io:format("Listening on port: ~p~n", [Port]);
    {error, _Reason} -> error
  end,
  tcp_accept(ListenSocket).

tcp_accept(ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} -> tcp_accept_ok(Socket);
    {error, _Reason} -> error
  end,
  gen_tcp:close(ListenSocket).

tcp_accept_ok(Socket) ->
  tcp_listen_socket(Socket).

tcp_listen_socket(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} -> tcp_recv(Data);
    {error, _Reason} -> error
  end.

tcp_recv(Data) ->
  io:format("~p ~p/~p ~p ~n", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Data]).