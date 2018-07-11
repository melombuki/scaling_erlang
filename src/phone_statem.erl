-module(phone_statem).
-behaviour(gen_statem).

-export([start_link/1, stop/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([inbound/1, action/2, busy/1, reject/1, accept/1, hangup/1]). %% Client API
-export([idle/3, calling/3, connected/3, receiving/3]). %% States

-define(SERVER, ?MODULE).

start_link(Ms) ->
  gen_statem:start_link(?SERVER, Ms, [{debug, [trace]}]).

stop(FsmPid) when is_pid(FsmPid)->
  hlr:detach(),
  gen_statem:stop(?SERVER).

terminate(_Reason, idle, _Ms) ->
  hlr:detach();
terminate(_Reason, calling, {_Ms, CallingMsId}) ->
  phone_statem:hangup(CallingMsId),
  hlr:detach();
terminate(_Reason, connected, {_Ms, OtherMsId, _Freq}) ->
  phone_statem:hangup(OtherMsId),
  hlr:detach();
terminate(_Reason, receiving, {_Ms, FromMsId}) ->
  phone_statem:reject(FromMsId),
  hlr:detach().

callback_mode() ->
  state_functions.

init(Ms) ->
  process_flag(trap_exit, true),
  hlr:attach(Ms),
  {ok, idle, Ms}.

action(ToMsId, {outbound, Ms}) ->
  gen_statem:call(ToMsId, {outbound, Ms});
action(ToMsId, Action) ->
  gen_statem:cast(ToMsId, {action, Action}).

busy(ToMsId) ->
  gen_statem:cast(ToMsId, {busy, self()}).
reject(ToMsId) ->
  gen_statem:cast(ToMsId, {reject, self()}).
accept(ToMsId) ->
  gen_statem:cast(ToMsId, {accept, self()}).
hangup(ToMsId) ->
  gen_statem:cast(ToMsId, {hangup, self()}).
inbound(ToMsId) ->
  gen_statem:cast(ToMsId, {inbound, self()}).

%% States
idle(cast, {inbound, FromMsId}, Ms) ->
  phone:reply(inbound, FromMsId, Ms),
  {next_state, receiving, {FromMsId, Ms}};
idle({call, From}, {outbound, ToMs}, Ms) ->
  case hlr:lookup_id(ToMs) of
    {error, invalid} ->
      phone:reply(invalid, ToMs, Ms),
      {keep_state_and_data, [{reply, From, "ERROR, INVALID"}]};
    {ok, ToMsId} ->
      phone:reply(outbound, ToMs, Ms),
      phone_statem:inbound(ToMsId),
      {next_state, calling, {Ms, ToMsId}, [{reply, From, "Calling..."}]}
  end;
idle(cast, Ignored, _Data) ->
  io:format("~p in idle, ignored: Event: ~w~n", [self(), Ignored]),
  keep_state_and_data;
idle({call, From}, Ignored, _Data) ->
  io:format("~p in idle, ignored: Event: ~w~n", [self(), Ignored]),
  {keep_state_and_data, [{reply, From, ok}]}.

calling(cast, {action, hangup}, {Ms, CallingMsId}) ->
  phone_statem:hangup(CallingMsId),
  {next_state, idle, Ms};
calling(cast, {busy, Pid}, {Ms, Pid}) ->
  phone:reply(busy, Pid, Ms),
  {next_state, idle, Ms};
calling(cast, {reject, Pid}, {Ms, Pid}) ->
  phone:reply(rejected, Pid, Ms),
  {next_state, idle, Ms};
calling(cast, {accept, Pid}, {Ms, Pid}) ->
  case frequency:allocate() of
    {error, no_frequency} ->
      phone_statem:reject(Pid),
      phone:reply(no_frequency, Pid, Ms),
      {next_state, idle, Ms};
    {ok, Freq} ->
      phone:reply(connected, Pid, Ms),
      {next_state, connected, {Ms, Pid, Freq}}
  end;
calling(cast, {inbound, Pid}, _Data) ->
  phone_statem:busy(Pid),
  keep_state_and_data;
calling(_, _, _) ->
  keep_state_and_data.

connected(cast, {inbound, FromMsId}, Data) ->
  phone:busy(FromMsId),
  {next_state, connected, Data};
connected(cast, {action, hangup}, {Ms, OtherMsId, Freq}) ->
  phone_statem:hangup(OtherMsId),
  frequency:deallocate(Freq),
  {next_state, idle, Ms};
connected(cast, {action, hangup}, {Ms, OtherMsId}) ->
  phone_statem:hangup(OtherMsId),
  {next_state, idle, Ms};
connected(cast, {hangup, OtherMsId}, {Ms, OtherMsId}) ->
  phone:reply(hangup, OtherMsId, Ms),
  {next_state, idle, Ms};
connected(cast, {hangup, OtherMsId}, {Ms, OtherMsId, Freq}) ->
  phone:reply(hangup, OtherMsId, Ms),
  frequency:deallocate(Freq),
  {next_state, connected, Ms};
connected(_, Ignored, _Data) ->
  io:format("~p in connected, ignored Event: ~w~n", [self(), Ignored]),
  keep_state_and_data.

receiving(cast, {action, accept}, {Ms, FromMsId}) ->
  phone_statem:accept(FromMsId),
  {next_state, connected, {Ms, FromMsId}};
receiving(cast, {action, reject}, {Ms, FromMsId}) ->
  phone_statem:reject(FromMsId),
  {next_state, idle, Ms};
receiving(cast, {hangup, FromMsId}, {Ms, FromMsId}) ->
  phone:reply(hangup, FromMsId, Ms),
  {next_state, idle, Ms};
receiving(cast, {inbound, FromMsId}, _Data) ->
  phone_statem:busy(FromMsId),
  keep_state_and_data;
receiving(cast, Ignored, _Data) ->
  io:format("~p in receiving, ignored: Event: ~w~n", [self(), Ignored]),
  keep_state_and_data;
receiving({call, From}, Ignored, _Data) ->
  io:format("~p in receiving, ignored: Event: ~w~n", [self(), Ignored]),
  {keep_state_and_data, [{reply, From, ok}]}.
