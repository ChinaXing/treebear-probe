-module(treebear_probe_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok,Host} = application:get_env(host),
    {ok,Port} = application:get_env(port),
    {ok,PackCount} = application:get_env(packCount),
    {ok,Parallel} = application:get_env(parallel),
    {ok,Count} = application:get_env(count),
    {ok,Delay} = application:get_env(delay),
    treebear_probe:start(Host, Port, Parallel, Count, Delay, PackCount).

stop(_State) ->
    ok.
