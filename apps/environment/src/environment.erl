-module(environment).
-behavior(application).

-export([start/2, stop/1]).

%%% -------------------------- Interface Functions ------------------------- %%%

start(normal, _Args) ->
  env_supervisor:start_link().

stop(_State) ->
  ok.