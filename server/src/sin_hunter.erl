-module(sin_hunter).

-export([
  spawn/2
]).
-export([get_pid/1,get_ref/1]).

-record(hunter,{
  config :: list(),
  pid :: pid(),
  ref :: reference(),
  parent :: pid(),
  delegate :: module()
}).
-type hunter() :: #hunter{}.

% start(HunterConfig) when erlang:is_list(HunterConfig) ->
%   case sin_conf:is_enabled(HunterConfig) of
%     true -> hunt(#hunter{hunter_config=HunterConfig});
%     false -> ok;
%     _ -> failed
%   end.

-callback handle_capture(Hunter :: hunter(), Socket :: gen_tcp:socket()) -> 
  {report, Report :: any()} |
  {noreport} |
  {noreport, Any :: any()}.

% ---

spawn(HunterConfig, Delegate) when erlang:is_list(HunterConfig)->
  Ref = erlang:make_ref(),
  ParentPid = erlang:self(),
  PreHunter = #hunter{config=HunterConfig, ref=Ref, parent=ParentPid, delegate=Delegate},
  Pid = erlang:spawn_link(fun () -> hunt(PreHunter) end),
  PreHunter#hunter{pid=Pid}.
  
get_pid(Hunter) ->
  Hunter#hunter.pid.

get_ref(Hunter) ->
  Hunter#hunter.ref.

% ---

hunt(PreHunter) ->
  Hunter = PreHunter#hunter{pid=self()},
  tcp_listen(Hunter).

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
    {ok, Port} -> io:format("Listening on port: ~p~n", [Port]);
    {error, _Reason} -> error
  end,
  loop(Hunter, ListenSocket).

loop(Hunter, ListenSocket) ->
  % io:format("Waiting for slaves...~n"),
  case gen_tcp:accept(ListenSocket, 0) of
    {ok, Socket} -> tcp_accept_ok(Hunter, Socket);
    {error, _Reason} -> error
  end,
  receive
    shutdown -> ok
  after
    0 -> loop(Hunter, ListenSocket)
  end,
  io:format("Closing socket...~n"),
  gen_tcp:close(ListenSocket).

tcp_accept_ok(Hunter, Socket) ->
  io:format("Slave connected!~n"),
  Result = (Hunter#hunter.delegate):handle_capture(Hunter, Socket),
  erlang:spawn(fun () -> handle_capture(Hunter, Result) end).

handle_capture(Hunter, {report, Report}) ->
  Hunter#hunter.parent ! Report;

handle_capture(_Hunter, {noreport, _Any}) ->
  ok;

handle_capture(_,_) ->
  ok.
