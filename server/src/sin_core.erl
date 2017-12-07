-module(sin_core).

-export([
  spawn/4
]).

spawn(Module, Function, Args) ->
  sin_proc:spawn().

spawn(MasterRef, Module, Function, Args) ->
  ok.