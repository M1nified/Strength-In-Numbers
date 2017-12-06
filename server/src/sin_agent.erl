-module(sin_agent).

-behaviour(gen_server).
-export([init/1]).
-export([handle_cast/2, handle_call/3, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([spawn/1]).

-record(state,{
  socket :: gen_tcp:socket()
}).

init(Args) when erlang:is_list(Args) ->
  case proplists:get_value(socket, Args) of
    undefined -> error;
    Socket -> init(Socket)
  end;

init(Socket) ->
  io:format("sin_agent init~n"),
  {ok, #state{socket=Socket}}.
  
handle_cast(Request, State) ->
  io:format("handle_cast: ~p~n", [Request]),
  {noreply, State}.

handle_call(Request, _From, State) ->
  io:format("handle_call: ~p~n", [Request]),
  {noreply, State}.

handle_info({tcp, _Socket, Msg}, State) ->
  io:format("handle_info tcp: ~p~n", [Msg]),
  {noreply, State}.

terminate(_Reason, _Tab) -> ok.

code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.

% ---

spawn(Socket) ->
  case gen_server:start_link(sin_agent, [{socket, Socket}], []) of
    {ok, Pid} -> Pid;
    {error, _Error} -> error;
    ignore -> ignore
  end.
