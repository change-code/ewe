-module(ewe_connection_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    {ok,
     {
       {simple_one_for_one, 0, 1},
       [
        { ewe_connection,
          {ewe_connection, start_link, []},
          temporary, brutal_kill, worker,
          [ewe_connection]
        }
       ]
     }
    }.
