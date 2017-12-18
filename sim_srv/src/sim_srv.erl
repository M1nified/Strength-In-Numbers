-module(sim_srv).

%% API exports
-export([main/1]).

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

start_services(ok, Args) ->
    select_role_and_run(lists:last(Args)),
    receive
        Any -> io:format("Rec ~p~n",[Any])
    end;
start_services(_RegisterResult, _Args) ->
    {error, "Failed."}.

select_role_and_run("master") ->
    io:format("Starting master server!~n"),
    {ok, LaborOffice} = sin_labor_office:open([{slaves_port, 3456}]),
    gen_server:cast(LaborOffice, {tcp_listen}),
    ok;
select_role_and_run("slave") ->
    io:format("Starting slave server!~n"),
    {ok, SlaveHead} = sin_slave_head:rise(),
    sin_slave_head:find_master(SlaveHead),
    ok;
select_role_and_run(_UnknownRole) ->
    {error, unknown_role}.
