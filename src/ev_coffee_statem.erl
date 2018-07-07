-module(ev_coffee_statem).
-behaviour(gen_statem).

-export([start_link/0, stop/0]).
-export([init/1, terminate/3, callback_mode/0, handle_event/4]).
-export([americano/0, cappuccino/0, tea/0, espresso/0,
         pay/1, cancel/0, cup_removed/0]).

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_statem:stop(?MODULE).

init([]) ->
  State = selection,
  Data = {},
  {ok, State, Data}.

callback_mode() ->
  handle_event_function.

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

handle_event({call, From}, {selection, Type, Price}, _State, _Data) ->
  hw:display("Please pay:~w", [Price]),
  {next_state, payment, {Type, Price, 0}, [{reply, From, ok}]};

handle_event({call, From}, {pay, Coin}, State, {Type, Price, Paid}) ->
  case State of
    selection ->
      hw:return_change(Coin),
      {keep_state_and_data, [{reply, From, selection}]};
    payment when Coin + Paid >= Price ->
      hw:display("Preparing Drink.", []),
      hw:return_change(Coin + Paid - Price),
      hw:drop_cup(), hw:prepare(Type),
      hw:display("Remove Drink.", []),
      {next_state, remove, {Type, Price, 0}, [{reply, From, ok}]};
    payment when Coin + Paid < Price ->
      ToPay = Price - (Coin + Paid),
      hw:display("Please pay:~w", [ToPay]),
      {next_state, payment, {Type, Price, Coin + Paid}, [{reply, From, ok}]};
    remove ->
      hw:return_change(Coin),
      keep_state_and_data
  end;

handle_event({call, From}, cancel, _State, {_, _, Paid}) ->
  hw:display("Make Your Selection", []),
  hw:return_change(Paid),
  {next_state, selection, {}, [{reply, From, ok}]};

handle_event({call, From}, cup_removed, _State, _Data) ->
  hw:display("Make Your Selection", []),
  {next_state, selection, {}, [{reply, From, ok}]};

handle_event(_, _, _, _) ->
  keep_state_and_data.

terminate(_Reason, _State, _Data) ->
  void.