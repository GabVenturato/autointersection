-module(vehicle_environment_test).



% Export startup functions.
-export([start_test/0]).

start_test() ->
  Start = "R_1",
  Finish = "R_30",
  {ok, Pid} = environment:start(),
  {ok, Vehicle} = vehicle:start_vehicle(get_route(Pid, {Start, Finish}), Pid),
  gen_server:call(Pid, {occupy_position, Vehicle, Start}),
  gen_server:call(Pid, {occupy_position, random_vehicle(), "R_4"}),
  gen_server:cast(coordinator, {startup}).


%% Internal functions

get_route(Pid, {Start, Finish}) ->
  gen_server:call(Pid, {get_route, {Start, Finish}}).

random_vehicle() ->
  Number = rand:uniform(1800),
  spawn(fun() -> timer:sleep(9000-Number),
                 gen_server:call(environment, {release_position, "R_4"}) end).


