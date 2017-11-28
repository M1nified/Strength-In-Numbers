-module(sin_xml).
-export([
  read/1
]).

-spec read(string()) -> {ok, any()} | {error, any()}.
read(ConfigFileName) ->
  case ct_config_xml:read_config(ConfigFileName) of
    {ok, Config} -> {ok, Config};
    _ -> {error,{{code,1},{details, {?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY}}}}
  end.

