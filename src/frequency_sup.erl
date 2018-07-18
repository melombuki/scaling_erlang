-module(frequency_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([stop/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  exit(whereis(?MODULE), shutdown).

init(_) ->
  ChildSpecList = [child(freq_overload), child(frequency)],
  {ok, {{rest_for_one, 2, 3600}, ChildSpecList}}.

child(Module) ->
  #{id => Module,
    start => {Module, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [Module]}.
