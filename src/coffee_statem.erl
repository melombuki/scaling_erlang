-module(coffee_statem).
-behaviour(gen_statem).

-export([start_link/0, stop/0]).
-export([init/1, terminate/3, callback_mode/0]).       % Callback functions
-export([selection/3, payment/3, remove/3]).           % States
-export([americano/0, cappuccino/0, tea/0, espresso/0, % Client functions
         pay/1, cancel/0, cup_removed/0]).

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Client Functions for Drink Selections
tea() -> 
  gen_statem:call(?MODULE, {selection, tea, 100}).
espresso() -> 
  gen_statem:call(?MODULE, {selection, espresso, 150}).
americano() -> 
  gen_statem:call(?MODULE, {selection, americano, 100}).
cappuccino() -> 
  gen_statem:call(?MODULE, {selection, cappuccino, 150}).

%% Client Functions for Actions
cup_removed() -> 
  gen_statem:call(?MODULE, cup_removed).
pay(Coin) -> 
  gen_statem:call(?MODULE, {pay, Coin}).
cancel() ->
   gen_statem:call(?MODULE, cancel).
stop() ->
  gen_statem:stop(?MODULE).

init([]) ->
  State = selection,
  Data = {},
  {ok, State, Data}.

callback_mode() ->
  state_functions.

%% State: selection
selection({call, From}, {selection, Type, Price}, _Data) ->
  hw:display("Please pay:~w", [Price]),
  {next_state, payment, {Type, Price, 0}, [{reply, From, ok}]};
selection({call, From}, {pay, Coin}, _Data) ->
  hw:return_change(Coin),
  {keep_state_and_data, [{reply, From, selection}]};
selection(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

%% State: payment
payment({call, From}, {pay, Coin}, {Type, Price, Paid}) when Coin + Paid >= Price ->
  hw:display("Preparing Drink.", []),
  hw:return_change(Coin + Paid - Price),
  hw:drop_cup(), hw:prepare(Type),
  hw:display("Remove Drink.", []),
  {next_state, remove, {Type, Price, 0}, [{reply, From, ok}]};
payment({call, From}, {pay, Coin}, {Type, Price, Paid}) when Coin + Paid < Price ->
  ToPay = Price - (Coin + Paid),
  hw:display("Please pay:~w", [ToPay]),
  {next_state, payment, {Type, Price, Coin + Paid}, [{reply, From, ok}]};
payment({call, From}, cancel, {_, _, Paid}) ->
  hw:display("Make Your Selection", []),
  hw:return_change(Paid),
  {next_state, selection, {}, [{reply, From, ok}]};
payment(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

%% State: remove
remove({call, From}, cup_removed, _Data) ->
  hw:display("Make Your Selection", []),
  {next_state, selection, {}, [{reply, From, ok}]};
remove({call, _From}, {pay, Coin}, _Data) ->
  hw:return_change(Coin),
  keep_state_and_data;
remove(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

terminate(_Reason, _State, _Data) ->
  void.

%% Handle events common to all states
handle_event({call, From}, _, _Data) ->
    %% Reply with the current count
    {keep_state_and_data, [{reply, From, ok}]};
handle_event(_EventType, _EventContent, _Data) ->
    %% Ignore all other events
    keep_state_and_data.