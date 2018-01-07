-module(sin_dep).

-export([needs/1]).
-export([fun_needs/3, fun_needs_modules/1, fun_needs_modules/3]).
-export([filter_loaded/1]).

needs(Modules) when erlang:is_list(Modules) ->
  Deps = lists:flatmap(fun (Module) -> needs(Module) end, Modules),
  lists:usort(Deps);

needs(Module) ->
  needs_deep(Module, []).

needs_deep([], ScannedModules) ->
  ScannedModules;

needs_deep([ModuleToScan], ScannedModules) ->
  needs_deep(ModuleToScan, ScannedModules) ++ [ModuleToScan];

needs_deep([ModuleToScan|ModulesToScan], ScannedModules) ->
  Scanned = needs_deep(ModuleToScan, ScannedModules),
  needs_deep(ModulesToScan, lists:usort(Scanned ++ [ModuleToScan]));

needs_deep(Module, ScannedModules) ->
  Default = ScannedModules ++ [Module],
  case lists:member(Module, ScannedModules) of
    true -> 
      Default;
    _ ->
      case code:which(Module) of
        preloaded -> Default;
        cover_compiled -> Default;
        non_existing -> Default;
        FileName -> 
          Imports = imports(FileName),
          needs_deep(Imports, ScannedModules ++ [Module])
      end
  end.

imports(FileName) ->
  case beam_lib:chunks(FileName, [imports]) of
    {ok, {_Module, ChunkResult}} -> 
      Chunks = proplists:get_value(imports, ChunkResult, []),
      chunks_to_modules(Chunks);
    {error, beam_lib, _Reason} -> error
  end.

chunks_to_modules(Chunks) when erlang:is_list(Chunks) ->
  Modules = lists:map(fun ({Module,_Function,_Arity}) -> Module end, Chunks),
  lists:usort(Modules).
  
% ---

fun_needs(Module, FunctionName, FunctionArity) ->
  fun_needs_deep([],[{Module, FunctionName, FunctionArity}]).

-spec fun_needs_modules({module(), term(), number()|list()}) -> [module()].
fun_needs_modules({Module, FunctionName, FunctionArityOrArguments}) when erlang:is_number(FunctionArityOrArguments) or erlang:is_list(FunctionArityOrArguments)->
  fun_needs_modules(Module, FunctionName, FunctionArityOrArguments).
  
-spec fun_needs_modules(module(), term(), number()|list()) -> [module()].
fun_needs_modules(Module, FunctionName, Arguments) when erlang:is_list(Arguments) ->
  fun_needs_modules(Module, FunctionName, list_count(Arguments));
fun_needs_modules(Module, FunctionName, FunctionArity) when erlang:is_number(FunctionArity) ->
  Funs = fun_needs(Module, FunctionName, FunctionArity),
  lists:usort([ Mod || {Mod, _, _} <- Funs]).

fun_needs_single(Module, FunctionName, Arguments) when erlang:is_list(Arguments) ->
  fun_needs_single(Module, FunctionName, list_count(Arguments));
fun_needs_single(Module, FunctionName, FunctionArity) when erlang:is_number(FunctionArity)->
  case code:which(Module) of
    non_existing -> 
      non_existing;
    cover_compiled -> 
      cover_compiled;
    preloaded -> 
      preloaded;
    Which ->
      case beam_disasm:file(Which) of
        {error, _, _} ->
          error;
        BeamFile ->
          {_,_,_,_,_,Functions} = BeamFile,
          Funs = lists:filter(fun ({_,Name, Arity, _Entry, _Code}) -> (Name == FunctionName) and  (Arity == FunctionArity)  end, Functions),
          case Funs of
            [Fun | _] ->
              {_, _Name, _Arity, _Entry, Code} = Fun,
              _ExternalCalls = lists:usort(lists:filtermap(fun filtermap_only_call_ext/1, Code));
            _ ->
              []
          end  
      end
  end.

fun_needs_deep(Checked, []) ->
  Checked;
fun_needs_deep(Checked, _Deps=[Current={Module, FunctionName, FunctionArity}|Pending]) ->
  CurrentNeeds = fun_needs_single(Module, FunctionName, FunctionArity),
  Unchecked = filter_unchecked(Checked, CurrentNeeds),
  fun_needs_deep(Checked ++ [Current], Pending ++ Unchecked).

filter_unchecked(_,error) -> [];
filter_unchecked(_,cover_compiled) -> [];
filter_unchecked(_,non_existing) -> [];
filter_unchecked(_,preloaded) -> [];
filter_unchecked(Checked, Needed) ->
  filter_unchecked(Checked, Needed, []).
filter_unchecked(_Checked, [], Unchecked) ->
  Unchecked;
filter_unchecked(Checked, [Current|Pending], Unchecked) ->
  case lists:member(Current, Checked) of
    true ->
      filter_unchecked(Checked, Pending, Unchecked);
    false ->
      filter_unchecked(Checked, Pending, Unchecked ++ [Current])
  end.

filtermap_only_call_ext({call_ext, _, {_, ExtModule, ExtFunName, ExtFunArity}}) -> {true, {ExtModule, ExtFunName, ExtFunArity}};
filtermap_only_call_ext(_) -> false.

% ---

is_loaded(Module) ->
  case code:is_loaded(Module) of
    false -> false;
    _ -> true
  end.

filter_loaded(Modules) when erlang:is_list(Modules) ->
  lists:filter(fun (Module) -> is_loaded(Module) end, Modules);

filter_loaded(_) ->
  [].

% ---

-spec list_count(list()) -> number().
list_count([]) -> 0;
list_count([_|Tail]) -> 1 + list_count(Tail).
