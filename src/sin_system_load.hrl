-record(sin_system_load,{
    cpu_avg_1 :: integer(),
    cpu_avg_5 :: integer(),
    cpu_avg_15 :: integer()
}).
-type sin_system_load() :: #sin_system_load{}.