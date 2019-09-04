-module(vehicle_environment_test).



% Export startup functions.
-export([start_test/0]).

start_test() ->
  Start = "R_1",
  Finish = "R_30",
  application:start(environment),
  application:start(vehicle),
  vehicle:initialize(get_route(Start, Finish), env),
  env:update_position(random_vehicle(), [], "R_4"),
  vehicle:startup().


%% Internal functions

get_route(Start, Finish) ->
  env:get_route(Start, Finish).

random_vehicle() ->
  Number = rand:uniform(1800),
  spawn(fun() -> timer:sleep(9000-Number),
                 env:release_position("R_4") end).


