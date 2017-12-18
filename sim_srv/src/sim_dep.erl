-module(sim_dep).

-export([needs/1]).

needs(Modules) when erlang:is_list(Modules) ->
  Deps = lists:flatmap(fun (Module) -> needs(Module) end, Modules),
  lists:usort(Deps);

needs(Module) ->
  needs_deep(Module, []).

needs_deep([], ScannedModules) ->
  % io:format("~n~n~p: ~p~n", [?LINE, ScannedModules]),
  % io:get_line(stop),
  ScannedModules;

needs_deep([ModuleToScan], ScannedModules) ->
  % io:format("~n~n~p: ~p~n", [?LINE, ScannedModules]),
  % io:get_line(stop),
  needs_deep(ModuleToScan, ScannedModules) ++ [ModuleToScan];

needs_deep([ModuleToScan|ModulesToScan], ScannedModules) ->
  % io:format("~n~n~p: [~p|~p] ~p~n", [?LINE, ModuleToScan, ModulesToScan, ScannedModules]),
  % io:get_line(stop),
  Scanned = needs_deep(ModuleToScan, ScannedModules),
  % io:format("Scanned: ~p~n", [Scanned]),
  % io:get_line(stop),
  needs_deep(ModulesToScan, lists:usort(Scanned ++ [ModuleToScan]));

needs_deep(Module, ScannedModules) ->
  % io:format("~p: SINGLE ~p ~p~n", [?LINE, Module, ScannedModules]),
  % io:get_line(stop),
  Default = ScannedModules ++ [Module],
  case lists:member(Module, ScannedModules) of
    true -> 
      % io:format("~p: TRUE~n", [?LINE]),
      Default;
    _ ->
      case code:which(Module) of
        preloaded -> Default;
        cover_compiled -> Default;
        non_existing -> Default;
        FileName -> 
          Imports = imports(FileName),
          % io:format("~p: ~p Imports: ~p~n", [?LINE, FileName, Imports]),
          % io:get_line(stop),
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
  