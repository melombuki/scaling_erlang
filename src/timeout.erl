-module(timeout).

-behaviour(gen_server).

-export([start/0, stop/0, timeout/1]).
-export([handle_call/3, init/1, handle_cast/2, handle_info/2]).

%% CLIENT FUNCTIONS

start() ->
  gen_server:start_link({local, timeout}, timeout, [], []).

stop() ->
  gen_server:cast(timeout, stop).

timeout(Ms) ->
  gen_server:call(timeout, {sleep, Ms}, 7000).

init(_Args) -> 
  {ok, undefined}.

handle_call({sleep, Ms}, _From, State) ->
   timer:sleep(Ms), 
  {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, State) ->
    {noreply, State}.