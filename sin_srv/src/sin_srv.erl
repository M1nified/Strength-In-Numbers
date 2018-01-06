-module(sin_srv).

%% API exports
-export([main/1]).
-export([start/1]).
-export([spawn/4]).
-export([master_pid/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    io:format("My PID is ~p~n", [self()]),
    io:format("CPU: ~p~n", [cpu_sup:avg1()]),
    start_services(register_single_instance(), Args),
    erlang:halt(0).

start(Args) ->
    {ok, Link, LinkRef} = sin_link:open(),
    io:format("Link: ~p~n",[Link]),
    start_services(register_single_instance(), Args, {Link, LinkRef}),
    {ok, Link, LinkRef}.

spawn({Link, LinkRef}, Module, Function, Args) ->
    try gen_server:call(Link, {LinkRef, {spawn, Module, Function, Args}}) of
        {ok, Pid} -> 
            Pid;
        _ -> 
            {error}
    catch
        _:Reason ->
            {error, Reason}
    end.

master_pid(Pid) ->
    sin_proc:master_pid(Pid).

%%====================================================================
%% Internal functions
%%====================================================================

register_single_instance() ->
    register(global:whereis_name(?MODULE)).

register(undefined) ->
    global:register_name(?MODULE, self()),
    ok;
register(_RunningServerPid) ->
    {error, "Server already running."}.

start_services(ok, [Mode|Options]) ->
    select_role_and_run(Mode, Options),
    receive
        Any -> io:format("Rec ~p~n",[Any])
    end;
start_services(_RegisterResult, _Options) ->
    {error, "Failed."}.

start_services(ok, [Mode|Options], {Link, LinkRef}) ->
    case select_role_and_run(Mode, Options) of
        {ok, Service} ->
            gen_server:cast(Link, {LinkRef, {service, Service}});
        _ -> fail
    end;
start_services(_RegisterResult, _Args, _) ->
    {error, "Failed."}.

select_role_and_run("master", Options) ->
    select_role_and_run(master, Options);
select_role_and_run("slave", Options) ->
    select_role_and_run(slave, Options);

select_role_and_run(master, Options) ->
    io:format("Starting master server!~n"),
    {ok, LaborOffice} = sin_labor_office:open(Options),
    gen_server:cast(LaborOffice, {tcp_listen}),
    {ok, LaborOffice};
select_role_and_run(slave, Options) ->
    io:format("Starting slave server!~n"),
    case sin_slave_head:rise(Options) of
        {ok, SlaveHead} ->
            sin_slave_head:find_master(SlaveHead),
            {ok, SlaveHead};
        _ -> error
    end;
select_role_and_run(_UnknownRole, _Options) ->
    {error, unknown_role}.
