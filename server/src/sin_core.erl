-module(sin_core).

-export([
  spawn/4
]).

spawn(MasterRef, Module, Function, Args) ->
