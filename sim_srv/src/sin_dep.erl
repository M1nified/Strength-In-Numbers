-module(sin_dep).

-export([needs/1]).
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

is_loaded(Module) ->
  case code:is_loaded(Module) of
    false -> false;
    _ -> true
  end.

filter_loaded(Modules) when erlang:is_list(Modules) ->
  lists:filter(fun (Module) -> is_loaded(Module) end, Modules);

filter_loaded(_) ->
  [].