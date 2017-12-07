-module(sim_srv).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    {ok, LaborOffice} = sin_labor_office:open([{slaves_port, 3456}]),
    gen_server:cast(LaborOffice, {tcp_listen}),
    receive
        Any -> io:format("Rec ~p~n",[Any])
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
