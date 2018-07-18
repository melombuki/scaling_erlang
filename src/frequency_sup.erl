-module(frequency_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([stop/0]).

stop() -> exit(whereis(?MODULE), shutdown).

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE},?MODULE, []),
    freq_overload:add(counters, {}),
    freq_overload:add(freq_logger, {file, "log"}),
    {ok, Pid}.

init(_) ->
    hlr:new(),
    ChildSpecList = [child(freq_overload), child(frequency)],
    {ok,{{rest_for_one, 2, 3600}, ChildSpecList}}.

child(Module) ->
    {Module, {Module, start_link, []},
     permanent, 2000, worker, [Module]}.
