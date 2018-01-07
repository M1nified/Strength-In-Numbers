-module(sin_code).

-export([module_to_binary/1,modules_to_binary/1]).
-export([load_module/1]).

-spec modules_to_binary([module()]) -> [{module(), binary(), file:filename()} | error].
modules_to_binary(Modules) when erlang:is_list(Modules) ->
  [ module_to_binary(Module) || Module <- Modules].  

-spec module_to_binary(Module :: module()) -> {Module :: module(), binary(), file:filename()} | error.
module_to_binary(Module) ->
  code:get_object_code(Module).

-spec load_module({module(), binary(), file:filename()}) -> ok | error.
load_module({Module, Binary, _Filename}) ->
  erlang:load_module(Module, Binary).