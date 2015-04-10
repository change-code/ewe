-module(ewe_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    ok = file:set_cwd(code:lib_dir(ewe, priv)),
    application:ensure_all_started(ewe).

start(_StartType, _StartArgs) ->
    ewe_sup:start_link().

stop(_State) ->
    ok.
