-module(ewe_listener).

-behaviour(supervisor_bridge).

-export([start_link/0]).

-export([init/1, terminate/2]).

-export([loop/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Port = application:get_env(ewe, port, 9999),
    {ok, Socket} =
        gen_tcp:listen(
          Port,
          [binary,
           {packet, http},
           {active, false},
           {reuseaddr, true}]),
    {ok, spawn_link(?MODULE, loop, [Socket]), Socket}.


loop(Socket) ->
    {ok, Conn} = gen_tcp:accept(Socket),
    Ref = make_ref(),
 
    case supervisor:start_child(ewe_connection_sup, [self(), Ref, Conn]) of
        {ok, _Pid} ->
            ok;
        {ok, _Pid, _} ->
            ok
    end,

    receive
        {handler, Ref, Handler} ->
            gen_tcp:controlling_process(Conn, Handler),
            Handler ! continue
    end,

    loop(Socket).


terminate(_Reason, _State) ->
    ok.
