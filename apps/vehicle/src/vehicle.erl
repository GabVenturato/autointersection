-module(vehicle).
-behavior(application).

-export([ start/2, stop/1, startup/0, initialize/2, set_testing_environment/1
        , cause_mechanical_failure/0]).

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

-define(LOGGING_COMPONENT(), 
  #component{name = "Default Logging Component",
             module = logging_component}).

-define(ENV_TESTING_COMPONENT(EnvPid), 
  #component{name = "Environment Testing Component",
             module = env_testing_component,
             probe = EnvPid}).

-include("component.hrl").

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
          ?MOTION_COMPONENT(EnvPid),
          ?LOGGING_COMPONENT()
        ]
      ).

set_testing_environment(EnvLocation) ->
  coordinator:set_testing_environment(?ENV_TESTING_COMPONENT(EnvLocation)).

cause_mechanical_failure() ->
  coordinator:cause_mechanical_failure().