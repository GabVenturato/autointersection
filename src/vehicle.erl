-module(vehicle).

-export([start/2, stop/1, start_vehicle/2]).

-define(RECOGNITION_COMPONENT(SensorPid), 
  #component{name = "Default Recognition Component",
             module = recognition_component,
             sensor = SensorPid}).

-define(COMMUNICATION_COMPONENT(SensorPid),
  #component{name = "Default Communication Component",
             module = communication_component,
             sensor = SensorPid}).

-define(MOTION_COMPONENT(SensorPid), 
  #component{name = "Default Motion Component",
             module = motion_component,
             sensor = SensorPid}).

-include("../include/component.hrl").

%%% -------------------------- Interface Functions ------------------------- %%%

start(normal, _Args) ->
    vehicle_supervisor:start_link().

stop(_State) ->
  ok.

start_vehicle(Route, EnvPid) ->
  vehicle_supervisor:start_link({
        Route, 
        [
          ?RECOGNITION_COMPONENT(EnvPid),
          ?COMMUNICATION_COMPONENT(EnvPid),
          ?MOTION_COMPONENT(EnvPid)
        ]
      }).