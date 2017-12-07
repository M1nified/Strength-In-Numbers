-module(sin_hunter).

-export([
  make/2
]).
-export([
  tcp_listen/1
]).

-record(hunter,{
  config :: list(),
  delegate :: module()
}).
-type hunter() :: #hunter{}.

% start(HunterConfig) when erlang:is_list(HunterConfig) ->
%   case sin_conf:is_enabled(HunterConfig) of
%     true -> hunt(#hunter{hunter_config=HunterConfig});
%     false -> ok;
%     _ -> failed
%   end.

-callback handle_accept(ListenSocket :: gen_tcp:socket()) ->
  ok | error | {error, Reason :: any()}.

% ---

make(HunterConfig, Delegate) when erlang:is_list(HunterConfig)->
  #hunter{config=HunterConfig, delegate=Delegate}.
  
% ---

tcp_listen(Hunter) ->
  Options = [
    binary,
    {packet, 4},
    {active, false},
    {keepalive, true},
    {port, proplists:get_value(slaves_port, Hunter#hunter.config, 0)},
    {ifaddr, proplists:get_value(interface_ip, Hunter#hunter.config, {127,0,0,1})},
    proplists:get_value(inet, Hunter#hunter.config, inet)
  ],
  io:format("tcp_listen options:~p~n", [Options]),
  case gen_tcp:listen(0, Options) of
    {ok, ListenSocket} -> tcp_listen_ok(Hunter, ListenSocket);
    {error, _Reason} -> error
  end.

tcp_listen_ok(Hunter, ListenSocket) ->
  case inet:port(ListenSocket) of
    {ok, Port} -> 
      io:format("Listening on port: ~p~n", [Port]),
      tcp_accept(Hunter, ListenSocket);
    {error, _Reason} -> error
  end.

tcp_accept(Hunter, ListenSocket) ->
  (Hunter#hunter.delegate):handle_accept(ListenSocket).