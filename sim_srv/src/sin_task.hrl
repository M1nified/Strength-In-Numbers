-record(sin_task,{
  spawn_3 :: {module(), atom(), [term()]},
  dependencies :: [module()]
}).
-type sin_task() :: #sin_task{}.

-record(sin_running_task,{
  module :: module(),
  function_name :: atom(),
  arguments :: [term()],
  pid :: pid(),
  sin_task :: sin_task()
}).
-type sin_running_task() :: #sin_running_task{}.