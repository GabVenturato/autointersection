-module( vehicle_generator ).
-behaviour( gen_server ).
-export( [start/2, start/3, start/4, start/5, start/6, start_link/2, stop/0] ).
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3] ).
-export( [terminate/2, generate_vehicles/3] ).

-define( HOSTNAME, element( 2, inet:gethostname() ) ).
-define( ENV_NODE_SHORT, list_to_atom( "env@" ++ ?HOSTNAME ) ).
-define( ENV_REF_SHORT, {env, ?ENV_NODE_SHORT} ).
-define( ENV_NODE_LONG, list_to_atom( "env@" ++ local_ip_v4() ) ).
-define( ENV_REF_LONG, {env, ?ENV_NODE_LONG} ).
-define( DEFAULT_MAX_FAIL_TIMEOUT, 20000 ).
-define( TOW_TRUCK_TIME, 30000 ).

-record( state,
  { env_node
  , env_ref
  , fail_ratio
  , sw_fail_rel_ratio
  , max_fail_timeout
  , node_list
  , start_list
  , finish_list
  }).

%%% -------------------------- Interface Functions ------------------------- %%%

%%% Functions 'start' and 'start_link' can be called both without parameters, or 
%%% specifying the path to the directory containing the config files.

start(N, FailRatio) ->
  gen_server:start(
    {local, ?MODULE},
    ?MODULE,
    [ N
    , FailRatio
    , 0.5
    , ?DEFAULT_MAX_FAIL_TIMEOUT
    , []
    , shortnames
    ],
    []
  ).

start(N, FailRatio, SwFailRelRatio) ->
  gen_server:start(
    {local, ?MODULE},
    ?MODULE,
    [ N
    , FailRatio
    , SwFailRelRatio
    , ?DEFAULT_MAX_FAIL_TIMEOUT
    , []
    , shortnames
    ],
    []
  ).

start(N, FailRatio, SwFailRelRatio, MaxFailTimeout) ->
  gen_server:start(
    {local, ?MODULE},
    ?MODULE,
    [ N
    , FailRatio
    , SwFailRelRatio
    , MaxFailTimeout
    , []
    , shortnames
    ],
    []
  ).

start(N, FailRatio, SwFailRelRatio, MaxFailTimeout, Nodes) ->
  gen_server:start(
    {local, ?MODULE},
    ?MODULE,
    [ N
    , FailRatio
    , SwFailRelRatio
    , MaxFailTimeout
    , Nodes
    , shortnames
    ],
    []
  ).

start(N, FailRatio, SwFailRelRatio, MaxFailTimeout, Nodes, NamesType) ->
  gen_server:start(
    {local, ?MODULE},
    ?MODULE,
    [ N
    , FailRatio
    , SwFailRelRatio
    , MaxFailTimeout
    , Nodes
    , NamesType
    ],
    []
  ).

start_link(N, FailRatio) ->
  gen_server:start_link(
    {local, ?MODULE},
    ?MODULE,
    [ N
    , FailRatio
    , 0.5
    , ?DEFAULT_MAX_FAIL_TIMEOUT
    , []
    , shortnames
    ],
    []
  ).

stop() ->
  gen_server:cast(?MODULE, stop).

%%% -------------------------- Callback Functions -------------------------- %%%

init([N, FailRatio, SwFailRelRatio, MaxFailTimeout, Nodes, NamesType]) ->
  {EnvNode, EnvRef} = case NamesType of
    longnames -> {?ENV_NODE_LONG, ?ENV_REF_LONG};
    _         -> {?ENV_NODE_SHORT, ?ENV_REF_SHORT}
  end,

  State = #state
    { env_node = EnvNode
    , env_ref = EnvRef
    , fail_ratio = FailRatio
    , sw_fail_rel_ratio = SwFailRelRatio
    , max_fail_timeout = MaxFailTimeout
    , node_list = Nodes
    , start_list = rpc:call( EnvNode, env, get_start_positions, [] )
    , finish_list = rpc:call( EnvNode, env, get_finish_positions, [] )
    },

  self() ! {generate_vehicles, N},

  {ok, State}.


%%% Synchronous and Asynchronous requests:

handle_call(_Msg, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.


%%% Handle info:

handle_info({generate_vehicles, N}, State) ->
  spawn_link( 
    ?MODULE,
    generate_vehicles,
    [N, State, self()]
  ),
  {noreply, State};

handle_info({cause_mechanical_failure, Node}, State) ->
  io:format( "GENERATOR: Causing mechanical failure to ~p~n", [Node] ),
  rpc:call( Node, vehicle, cause_mechanical_failure, [] ),
  {noreply, State};

handle_info({casue_software_failure, Node}, State) ->
  io:format( "GENERATOR: Causign software failure to ~p~n", [Node] ),
  rpc:call( Node, application, stop, [vehicle] ),
  {noreply, State};

handle_info(_Msg, State) -> {noreply, State}.


%%% Code change:

code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behavior, but will not be 
  %% used.
  {ok, State}.


%%% Stop the environment process

terminate(Reason, _Env) -> {ok, Reason}.

%%% -------------------------- Private Functions --------------------------- %%%

generate_vehicles(0, _, _) -> ok;

generate_vehicles(
    I,
    #state
      { env_node = EnvNode
      , env_ref = EnvRef
      , fail_ratio = FailRatio
      , sw_fail_rel_ratio = SwFailRelRatio
      , max_fail_timeout = MaxFailTimeout
      , node_list = Nodes
      , start_list = StartList
      , finish_list = FinishList
      } = State,
    GenPid
  ) ->

  generate_vehicles(
    I - 1,
    State#state{node_list = safe_tail( Nodes )}, 
    GenPid
  ),

  Start = lists:nth( rand:uniform( length( StartList ) ), StartList ),
  Finish = lists:nth( rand:uniform( length( FinishList ) ), FinishList ),
  Route = rpc:call( EnvNode, env, get_route, [Start, Finish]),

  %% If node_list is defined take node name from it (assuming the node is 
  %%  already started), otherwise create one.
  Vehicle = case Nodes of
    [] ->
      Name = "v" ++ erlang:integer_to_list( I ),
      {ok, Node} = slave:start( 
        ?HOSTNAME, 
        Name, 
        "-pa ebin" 
      ),
      Node;
    _ ->
      erlang:hd( Nodes )
  end,
  
  rpc:call( Vehicle, application, start, [vehicle] ),
  rpc:call( Vehicle, vehicle, initialize, [Route, EnvRef] ),
  rpc:call( Vehicle, vehicle, set_testing_environment, [EnvRef] ),

  io:format( 
    "GENERATOR: Vehice ~p generated! Starting from ~p and directed to ~p~n", 
    [Vehicle, Start, Finish]
  ),

  % Startup the vehicle
  startup( Vehicle, Start, null, EnvNode, EnvRef ),

  C = rand:uniform(),
  if 
    % Make it fail
    C < FailRatio ->
      F = rand:uniform(), % choose failure type
      if 
        % Cause a software failure
        F < SwFailRelRatio -> 
          erlang:send_after( 
            rand:uniform( MaxFailTimeout ),
            GenPid,
            {casue_software_failure, Vehicle}
          );

        % Cause a mechanical failure
        true -> 
          erlang:send_after( 
            rand:uniform( MaxFailTimeout ),
            GenPid,
            {cause_mechanical_failure, Vehicle}
          )
      end;

    % Let it run
    true -> nothing
  end.

startup(Vehicle, StartPos, Ref, EnvNode, EnvRef) ->
  receive
    {'DOWN', Ref, process, {_, V}, Reason} ->
      case Reason of
        normal -> nothing;
        _ ->
          io:format( 
            "GENEATOR: Vehicle in front has software failure!" ++
            " Waiting for the tow truck...~n"
          ),
          gen_server:cast( EnvRef, {vehicle_down, V} ),
          timer:sleep( ?TOW_TRUCK_TIME ),
          startup( Vehicle, StartPos, null, EnvNode, EnvRef )
      end
  after 500 ->
    case rpc:call( EnvNode, env, is_position_free, [StartPos] ) of
      true -> % position is free
        rpc:call( Vehicle, vehicle, startup, [] );
      _    -> % position is occupied: monitor vehicle
        Res = gen_server:call( EnvRef, {vehicle_at, StartPos} ),
        case Res of
          {vehicle, V} ->
            NewRef = erlang:monitor( process, {vehicle_supervisor, V} ),
            startup( Vehicle, StartPos, NewRef, EnvNode, EnvRef );
          _ ->
            startup( Vehicle, StartPos, null, EnvNode, EnvRef )
        end
    end
  end.


safe_tail([]) -> [];
safe_tail([_ | Tail])  -> Tail.

local_ip_v4() ->
  {ok, Addrs} = inet:getifaddrs(),
  Ip = hd([
        Addr || {_, Opts} <- Addrs, {addr, Addr} <- Opts,
        size(Addr) == 4, Addr =/= {127,0,0,1}
  ]),
  case Ip of
    {N1,N2,N3,N4} -> lists:concat([N1, ".", N2, ".", N3, ".", N4]);
    _ -> null
  end.