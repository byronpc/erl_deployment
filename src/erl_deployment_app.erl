-module(erl_deployment_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    sd_notify:sd_notify(0,"READY=1"),
    erl_deployment_sup:start_link().

stop(_State) ->
    ok.
