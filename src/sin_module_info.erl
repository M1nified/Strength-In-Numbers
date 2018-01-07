-module(sin_module_info).

-author("Michał Góra").
-vsn("1.0").

-export([get_module_version/1, is_version/2]).

get_module_version(Module) ->
  Info = Module:module_info(),
  case proplists:get_value(attributes, Info) of
    undefined -> undefined;
    Attributes ->
      proplists:get_value(vsn, Attributes)
  end.

is_version(Module, DesiredVersion) ->
  case get_module_version(Module) of
    DesiredVersion ->
      true;
    Version ->
      % io:format("VERSION CMP: ~p ~p ~p~n", [Module, DesiredVersion, Version]),
      false
  end.