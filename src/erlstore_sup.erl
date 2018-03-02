
-module(erlstore_sup).

-behaviour(supervisor).

-export([start_link/0,init/1]).

start_link() ->
    supervisor:start_link(
        {local, ?MODULE}, ?MODULE, []
    ).

init([]) -> 
    Procs = [],
    {ok, {{one_for_one, 10, 5}, Procs}}.