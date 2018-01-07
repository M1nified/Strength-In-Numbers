-module(sin_proc).


-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([pid/1, master_pid/1, add_client/1]).

-include("./sin_debug.hrl").

-record(state,{
  fake_ref :: reference(),
  clients :: [pid()],
  master_pids :: [{pid(), pid()}] % {master_pid, fake_pid}
}).

init(_Args) ->
  ?DBG_INFO("[~p:~p]~n",[?MODULE,?FUNCTION_NAME]),
  {ok, #state{master_pids=[], fake_ref=erlang:make_ref(), clients=[]}}.

handle_cast({add_client, ClientPid}, State=#state{clients=Clients}) ->
  ?DBG_INFO("[~p:~p][add_client] Client: ~p~n",[?MODULE,?FUNCTION_NAME, ClientPid]),
  case lists:member(ClientPid, Clients) of
    true ->
      ?DBG_INFO("[~p:~p][add_client] true: ~p~n",[?MODULE,?FUNCTION_NAME, ClientPid]),
      {noreply, State};
    _ ->
      ?DBG_INFO("[~p:~p][add_client] false: ~p~n",[?MODULE,?FUNCTION_NAME, ClientPid]),
      {noreply, State#state{clients=Clients++[ClientPid]}}
  end;

handle_cast({fake_proc, FakeProc, OriginalPid, {msg, Msg}}, State=#state{master_pids=MasterPids}) when erlang:is_pid(FakeProc) and erlang:is_pid(OriginalPid) ->
  ?DBG_INFO("[~p:~p][fake_proc] Request: ~p~n",[?MODULE,?FUNCTION_NAME, {fake_proc, FakeProc, OriginalPid, Msg}]),
  case proplists:get_value(OriginalPid, MasterPids) of
    undefined ->
      ?DBG_INFO("[~p:~p][fake_proc] undefined~n",[?MODULE,?FUNCTION_NAME]),
      send_to_all_clients({?MODULE, captured_message, to_master, OriginalPid, Msg}, State),
      {noreply, State};
    FakeProc ->
      ?DBG_INFO("[~p:~p][fake_proc] FakeProc: ~p~n",[?MODULE,?FUNCTION_NAME, FakeProc]),
      send_to_all_clients({?MODULE, captured_message, to_master, OriginalPid, Msg}, State),
      {noreply, State};
    Else ->
      ?DBG_INFO("[~p:~p][fake_proc] Else: ~p~n",[?MODULE,?FUNCTION_NAME, Else]),
      {noreply, State}
  end;

handle_cast(Request, State) ->
  ?DBG_INFO("[~p:~p] Request: ~p~n",[?MODULE,?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_call({get_fake_master_pid, OriginalPid}, _From, State=#state{master_pids=MasterPids}) ->
  case proplists:get_value(OriginalPid, MasterPids) of
    undefined ->
      FakePid = make_fake_proc(OriginalPid),
      {reply, FakePid, State#state{master_pids=MasterPids++[{OriginalPid,FakePid}]}};
    FakePid ->
      {reply, FakePid, State}
  end;

handle_call(Request, _From, State) ->
  ?DBG_INFO("[~p:~p] Request: ~p~n",[?MODULE,?FUNCTION_NAME, Request]),
  {noreply, State}.

handle_info(Request, State) ->
  ?DBG_INFO("[~p:~p] Request: ~p~n",[?MODULE,?FUNCTION_NAME, Request]),
  {noreply, State}.

terminate(_Reason, _Tab) -> ok.
  
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.


% --- API public

-spec pid(pid()) -> pid().
pid(Pid) ->
  Srv = get_proc_srv(),
  FakePid = get_fake_pid(Srv, Pid),
  FakePid.

-spec master_pid(pid()) -> pid().
master_pid(Pid) ->
  Srv = get_proc_srv(),
  get_fake_master_pid(Srv, Pid).

-spec add_client(pid()) -> ok.
add_client(ClientPid) ->
  Srv = get_proc_srv(),
  gen_server:cast(Srv, {add_client, ClientPid}).

% --- API local

-spec get_fake_pid(pid(),pid()) -> pid().
get_fake_pid(Srv, Pid) ->
  gen_server:call(Srv, {get_fake_pid, Pid}).

-spec get_fake_master_pid(pid(),pid()) -> pid().
get_fake_master_pid(Srv, Pid) ->
  gen_server:call(Srv, {get_fake_master_pid, Pid}).
  
-spec get_original_pid(pid(),pid()) -> pid().
get_original_pid(Srv, Pid) ->
  gen_server:call(Srv, {get_original_pid, Pid}).

% ---

start() ->
  gen_server:start(?MODULE, [], []).

get_proc_srv() ->
  case erlang:whereis(?MODULE) of
    undefined ->
      case start() of
        {ok, Pid} ->
          erlang:register(?MODULE, Pid),
          Pid;
        _ -> error
      end;
    Pid ->
      Pid
  end.

send_to_all_clients(Msg, State=#state{clients=Clients}) ->
  ?DBG_INFO("[~p:~p] State: ~p~n",[?MODULE,?FUNCTION_NAME, State]),
  lists:foreach(fun (Pid) -> Pid ! Msg end, Clients).
      
make_fake_proc(OriginalPid) ->
  erlang:spawn(fun () -> fake_proc_main(OriginalPid) end).

fake_proc_main(OriginalPid) ->
  fake_proc_loop({OriginalPid}).

fake_proc_loop(State={OriginalPid}) ->
  receive
    Msg ->
      ?DBG_INFO("[~p:~p] Msg: ~p~n",[?MODULE,?FUNCTION_NAME, Msg]),
      gen_server:cast(get_proc_srv(), {fake_proc, self(), OriginalPid, {msg, Msg}}),
      fake_proc_loop(State)
  end.