-module(sin_conf).
-export([
  read/1,
  get_slave_conf/1,
  get_master_conf/1,
  is_enabled/1
]).

-spec read(string()) -> {ok, any()} | {error, any()}.
read(ConfigFileName) ->
  case ct_config_xml:read_config(ConfigFileName) of
    {ok, Config} -> {ok, Config};
    _ -> {error,{{code,1},{details, {?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY}}}}
  end.

get_slave_conf(Config) ->
  proplists:get_value(slave_config, Config, [{enabled, false}]).

get_master_conf(Config) ->
  proplists:get_value(master_config, Config, [{enabled, false}]).

is_enabled(ElementConfig) when erlang:is_list(ElementConfig) ->
  proplists:get_value(enabled, ElementConfig, true).