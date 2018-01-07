-type sin_spawn_3() :: {module(), atom(), [term()]}.

-record(sin_task,{
  ref :: reference(),
  spawn_3 :: sin_spawn_3(),
  dependencies :: [module()]
}).
-type sin_task() :: #sin_task{}.

-record(sin_running_task,{
  pid :: pid(),
  execution_ref :: reference(),
  task :: sin_task()
}).
-type sin_running_task() :: #sin_running_task{}.

-record(sin_task_sim, {
  pid :: pid(),
  task :: sin_task()
}).
-type sin_task_sim() :: #sin_task_sim{}.
