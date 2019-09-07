-module( vehicle_generator ).
-export([generate/2, generate/3, generate/4, manage/5]).

-define( HOSTNAME, element( 2, inet:gethostname() ) ).
-define( ENV_NODE, list_to_atom( "env@" ++ ?HOSTNAME ) ).
-define( ENV_REF, {env, ?ENV_NODE} ).
-define( DEFAULT_MAX_CRASH_TIMEOUT, 10000 ).

generate(N, FailRatio) ->
  generate( N, FailRatio, 0.5, ?DEFAULT_MAX_CRASH_TIMEOUT ).

generate(N, FailRatio, SwFailRelRatio) ->
  generate( N, FailRatio, SwFailRelRatio, ?DEFAULT_MAX_CRASH_TIMEOUT ).

generate(N, FailRatio, SwFailRelRatio, MaxFailTimeout) ->
  StartList = rpc:call( ?ENV_NODE, env, get_start_positions, [] ),
  FinishList = rpc:call( ?ENV_NODE, env, get_finish_positions, [] ),
  Vehicles = generate_vehicles( N, StartList, FinishList ),
  lists:map( 
    fun({V,S}) -> 
      spawn_link( 
        node(), 
        ?MODULE,
        manage,
        [V, S, FailRatio, SwFailRelRatio, MaxFailTimeout]
      ) 
    end,
    Vehicles
  ).

generate_vehicles(0, _, _) -> [];
generate_vehicles(I, StartList, FinishList) ->
  StartedVehicles = generate_vehicles( I - 1, StartList, FinishList ),

  Start = lists:nth( rand:uniform( length( StartList ) ), StartList ),
  Finish = lists:nth( rand:uniform( length( FinishList ) ), FinishList ),
  Route = rpc:call( ?ENV_NODE, env, get_route, [Start, Finish]),

  Name = "v" ++ erlang:integer_to_list(I),
  {ok, Node} = slave:start( ?HOSTNAME, Name, "-pa ebin" ),
  
  rpc:call( Node, application, start, [vehicle] ),
  rpc:call( Node, vehicle, initialize, [Route, ?ENV_REF] ),
  rpc:call( Node, vehicle, set_testing_environment, [?ENV_REF] ),

  [ {Node, Start} | StartedVehicles ].

manage(Vehicle, StartPos, FailRatio, SwFailRelRatio, MaxFailTimeout) ->
  % Startup the vehicle
  startup( Vehicle, StartPos ),

  C = rand:uniform(),
  if 
    % Make it fail
    C < FailRatio ->
      timer:sleep( rand:uniform( MaxFailTimeout ) ), % wait before fail
      F = rand:uniform(), % choose failure type
      if 
        % Cause a software failure
        F < SwFailRelRatio -> rpc:call( Vehicle, application, stop, [vehicle] );

        % Cause a mechanical failure
        true -> rpc:call( Vehicle, vehicle, cause_mechanical_failure, [] )
      end;

    % Let it run
    true -> nothing
  end.

startup(Vehicle, StartPos) ->
  case rpc:call( ?ENV_NODE, env, is_position_free, [StartPos] ) of
    true -> rpc:call( Vehicle, vehicle, startup, [] );
    _    -> timer:sleep( 300 ), startup( Vehicle, StartPos )
  end.