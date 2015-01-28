-module(ewe_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs =
        [
         { ewe_listener,
           {ewe_listener, start_link, []},
           permanent, brutal_kill, worker,
           [ewe_listener]
         },
         { ewe_connection_sup,
           {ewe_connection_sup, start_link, []},
           permanent, brutal_kill, worker,
           [ewe_connection_sup]
         }
        ],

    {ok, {SupFlags, ChildSpecs}}.
