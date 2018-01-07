-module(sin_system_load).

-export([start/0,stop/0]).
-export([get_system_load/0]).

-include("./sin_system_load.hrl").

start() ->
    cpu_sup:start().

stop() -> 
    cpu_sup:stop().

get_system_load() ->
    {Avg1, Avg5, Avg15} = get_cpu_loads(),
    #sin_system_load{
        cpu_avg_1=Avg1,
        cpu_avg_5=Avg5,
        cpu_avg_15=Avg15
    }.

get_cpu_loads() ->
    {cpu_sup:avg1(), cpu_sup:avg5(), cpu_sup:avg15()}.