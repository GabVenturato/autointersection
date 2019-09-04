-module(vehicle).
-behavior(application).

-export([start/2, stop/1, startup/0, initialize/2, set_testing_environment/1]).

-define(RECOGNITION_COMPONENT(ProbePid), 
  #component{name = "Default Recognition Component",
             module = recognition_component,
             probe = ProbePid}).

-define(COMMUNICATION_COMPONENT(ProbePid),
  #component{name = "Default Communication Component",
             module = communication_component,
             probe = ProbePid}).

-define(MOTION_COMPONENT(ProbePid), 
  #component{name = "Default Motion Component",
             module = motion_component,
             probe = ProbePid}).

-include("../include/component.hrl").

%%% -------------------------- Interface Functions ------------------------- %%%

start(normal, _Args) ->
    vehicle_supervisor:start_link().

stop(_State) ->
  ok.

startup() ->
  coordinator:startup().

initialize(Route, EnvPid) ->
  coordinator:initialize(
        Route, 
        [
          ?RECOGNITION_COMPONENT(EnvPid),
          ?COMMUNICATION_COMPONENT(EnvPid),
          ?MOTION_COMPONENT(EnvPid)
        ]
      ).

set_testing_environment(EnvLocation) ->
  coordinator:set_testing_environment(EnvLocation).