-module( vehicle_generator ).

-define( HOSTNAME, element(2,inet:gethostname()) ).
-define( ENV_NODE, "env@" ++ ?HOSTNAME )
-define( ENV_REF, {env, ?ENV_NODE} ).

generate( N, CrashRatio ) -> ok.


start_vehicle( I, StartList, FinishList ) ->
  Start = lists:nth( rand:uniform( length( StartList ) ), StartList ),
  Finish = lists:nth( rand:uniform( length( FinishList ) ), FinishList ),
  Route = rpc:call( ENV_NODE, env, get_route, [Start, Finish]).


  % application:start(vehicle).
  % vehicle:initialize(Route, {env, 'env@home-pc'}).
  % vehicle:set_testing_environment({env, 'env@home-pc'}).
  % vehicle:startup().