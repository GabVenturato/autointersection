-module(intersection_coordination).
-behavior(gen_statem).

-export([start/2, start_link/2, callback_mode/0]).
-export([init/1, terminate/3, handle_common/3]).
-export([ready/3, election/3, crossing/3]).

-record( cross,
  { probe               % sensors reference (i.e. Pid / info to contact them)
  , ev_man              % event manager reference
  , nonce               % note: ID = (nonce, wait_counter)
  , wait_counter = 0    % how many vehicles crossed before me
  , participants        % list that include myself
  , role = simple       % possible values: simple | leader
  , leader = null       % leader reference
  , candidate = null    % candidate reference
  , monitor_ref = null  % leader monitor 
  }).

-define( TIMEOUT_ELECTION, 2 * 1000 ). % milliseconds
-define( TIMEOUT_ANSWER, ?TIMEOUT_ELECTION * 2 ).
-define( SELF_REF, {?MODULE, node()} ).
-define( SUP_MODULE, vehicle_supervisor ).

-define( EVENT_HANDLER_ID, {ic_event_handler, make_ref()} ).

-define( HANDLE_COMMON, ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D) ).

%%% -------------------------- Interface Functions ------------------------- %%%

start(Probe, EvMan) ->
  gen_statem:start({local, ?MODULE}, ?MODULE, [Probe, EvMan], []).

start_link(Probe, EvMan) ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [Probe, EvMan], []).


callback_mode() ->
  state_functions.

%%% -------------------------- Callback Functions -------------------------- %%%

init([Probe, EvMan]) ->
  process_flag(trap_exit, true),
  gen_event:add_handler(EvMan, ?EVENT_HANDLER_ID, [self()]),

  Data = #cross
    { probe = Probe
    , ev_man = EvMan
    , nonce = generate_nonce()
    , participants = get_participants( Probe )
    , monitor_ref = make_ref() % random ref to not cause badarg in demonitor
    },
  
  Participants = participants( Data ),
  AllParticipants = Participants ++ lists:map(
    fun get_reference/1,
    gen_server:call(Probe, get_crossing_participants)
  ),

  case AllParticipants of
    [] -> % If there are no participants, I'm the leader and start crossing
      internal:a_log( 
        Data#cross.ev_man,
        info,
        "Intersection coordination",
        "No participants, I'm the leader!"
      ),
      internal:event( Data#cross.ev_man, notification, position_type, normal ),
      {ok, crossing, Data#cross{ role = leader }};
    _  -> % otherwise ask who is the leader
      cast_vehicles( AllParticipants, {whos_leader, ?SELF_REF} ),
      { ok
      , ready
      , Data
      , [ {{timeout, need_election}, ?TIMEOUT_ANSWER, need_election}
        ]
      }
  end.

terminate(Reason, _State, Data) ->
  gen_event:delete_handler( Data#cross.ev_man, ?EVENT_HANDLER_ID, [] ),
  {ok, Reason}.


%%% STATE: READY
%%%  Vehicle ready to cross but it is not the leader, waiting its turn.

%% Timout need_election expired: an election is needed.
ready({timeout, need_election}, need_election, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    debug,
    "Intersection coordination",
    "Timeout expired, election needed."
  ),
  cast_vehicles( 
    participants( Data ),
    {election, ?SELF_REF, get_my_id( Data )}
  ),
  { next_state
  , election
  , Data
  , [ {{timeout, election_expired}, ?TIMEOUT_ELECTION, election_expired}
    ]
  };

%% New leader received (when a candidate becomes the new leader or if joining.
%%  when someone just winned an election)
ready(cast, {coordinator, {_, Node} = L}, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    info,
    "Intersection coordination",
    lists:concat( ["Found leader: ", Node] )
  ),
  MonitorRef = erlang:monitor( process, {?SUP_MODULE, Node} ),
  { keep_state
  , Data#cross{ leader = L, monitor_ref = MonitorRef }
  , [ {{timeout, need_election}, infinity, undefined}
    ]
  };

%% Received a request to perform an election, postpone it to "election" state.
ready(cast, {election, _From, _Id}, Data) ->
  { next_state
  , election
  , Data
  , [ postpone
    , {{timeout, need_election}, infinity, undefined}
    ]
  };

%% Received a promotion message. I am going to be the new leader.
ready(cast, promotion, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    info,
    "Intersection coordination",
    "Promotion received. I'm the new leader."
  ),
  cast_vehicles( participants( Data ), {coordinator, ?SELF_REF} ),
  internal:event( Data#cross.ev_man, notification, position_type, normal ),
  { next_state
  , crossing
  , Data#cross
      { role = leader
      , candidate = clockwise_next( Data#cross.participants, ?SELF_REF )
      , wait_counter = gen_server:call( Data#cross.probe, get_entrance_number )
      }
  , [ {{timeout, need_election}, infinity, undefined}
    ]
  };

?HANDLE_COMMON.


%%% STATE: ELECTION
%%%   Participants need to find a leader which will starts to cross. A slightly
%%%   modified version of the Bully Algorithm is applied here: the IDs are not
%%%   known a priori. Still, it guarantees anyway safety and liveness.

%% Election message was sent, but no one answered, so I am the leader.
election({timeout, election_expired}, election_expired, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    debug,
    "Intersection coordination",
    "No one answered my election message. I'm the leader."
  ),
  cast_vehicles( participants( Data ), {coordinator, ?SELF_REF} ),
  internal:event( Data#cross.ev_man, notification, position_type, normal ),
  { next_state
  , crossing
  , Data#cross
      { role = leader
      , candidate = clockwise_next( Data#cross.participants, ?SELF_REF )
      }
  };

%% Expected to receive coordinator message but none arrived, so do new election.
election({timeout, answer_expired}, answer_expired, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    debug,
    "Intersection coordination",
    "Answer expired, election needed."
  ),
  cast_vehicles(
    participants( Data ),
    {election, ?SELF_REF, get_my_id( Data )}
  ),
  { keep_state_and_data
  , [ {{timeout, election_expired}, ?TIMEOUT_ELECTION, election_expired}
    ]
  };

%% Election message received: forward the election only if myID > receivedID.
election(cast, {election, From, Id}, Data) ->
  LogMsg = "Election message received.",
  MyId = get_my_id( Data ),
  Gt = gt_id( MyId, Id ),
  case Gt of
    true -> 
      internal:a_log( 
        Data#cross.ev_man,
        debug,
        "Intersection coordination",
        LogMsg ++ " Forward election."
      ),
      gen_statem:cast( From, answer ),
      cast_vehicles( 
        participants( Data ),
        {election, ?SELF_REF, MyId}
      ),
      { keep_state_and_data
      , [ {{timeout, election_expired}, ?TIMEOUT_ELECTION, election_expired}
        ]
      };
    _    ->
      internal:a_log( 
        Data#cross.ev_man,
        debug,
        "Intersection coordination",
        LogMsg ++ " Not forward election."
      ),
      { keep_state_and_data
      , [ {{timeout, answer_expired}, ?TIMEOUT_ANSWER, answer_expired}
        ]
      }
  end;

%% Answer message received: I am not the one with the greatest ID.
election(cast, answer, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    debug,
    "Intersection coordination",
    "Received answer. Wait for coordinator message."
  ),
  { keep_state_and_data
  , [ {{timeout, election_expired}, infinity, undefined}
    , {{timeout, answer_expired}, ?TIMEOUT_ANSWER, answer_expired}
    ]
  };

%% Coordinator message received. Leader is elected, monitor it and conclude 
%%  the election algorithm.
election(cast, {coordinator, {_, Node} = L}, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    info,
    "Intersection coordination",
    lists:concat( ["Election complete! Found leader: ", Node] )
  ),
  MonitorRef = erlang:monitor( process, {?SUP_MODULE, Node} ),
  { next_state
  , ready
  , Data#cross{ leader = L, monitor_ref = MonitorRef }
  , [ {{timeout, answer_expired}, infinity, undefined}
    , {{timeout, election_expired}, infinity, undefined}
    ]
  };

%% Already performing an election, ignore.
election({timeout, need_election}, need_election, _Data) ->
  keep_state_and_data;

?HANDLE_COMMON.


%%% STATE: CROSSING
%%%   When the leader is crossing. This state doesn't have the handle_common
%%%   part, so some messages are handled both here and below in handle_common
%%%   function: whos_leader, mechanical_failure.

%% I have completed the crossing: notify all participants and promote the
%%  candidate.
crossing(cast, crossing_complete, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    debug,
    "Intersection coordination",
    lists:concat(
      [ "Crossing complete! Pass the lead to "
      , Data#cross.candidate]
    )
  ),
  cast_vehicles( participants( Data ), crossed ),
  gen_statem:cast( Data#cross.candidate, promotion ),
  {stop, normal};

%% New vehicle arrived at the intersection and asked who is the new leader:
%%  I'm the leader, so I answer. Participants are also updated.
crossing(cast, {whos_leader, From}, Data) ->
  gen_statem:cast( From, {coordinator, ?SELF_REF} ),
  Participants = get_participants( Data#cross.probe ),
  {keep_state, Data#cross{participants = Participants}};

%% I have a mechanical failure: pretend to have crossed and promote the 
%%  candidate to new leader. Further details in handle_common below.
crossing(cast, mechanical_failure, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    debug,
    "Intersection coordination",
    lists:concat(
      [ "Mechanical failure detected. Pass the lead to "
      , Data#cross.candidate]
    )
  ),
  cast_vehicles( participants( Data ), crossed ), % pretend to have crossed
  gen_statem:cast( Data#cross.candidate, promotion ),
  {stop, normal};

%% If an election message is received, notify to all that I'm still the leader.
crossing(cast, {election, _From, _Id}, Data) ->
  cast_vehicles( participants( Data ), {coordinator, ?SELF_REF} ),
  keep_state_and_data;

crossing(cast, Msg, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    debug,
    "Intersection coordination",
    lists:concat( ["Ignoring message: ", Msg] )
  ),
  keep_state_and_data.


%%% HANDLE COMMON

%% New vehicle arrived at the intersection and asked who is the new leader:
%%  answere only if I am the leader.
handle_common(cast, {whos_leader, From}, Data) ->
  case Data#cross.role of
    leader -> gen_statem:cast( From, {coordinator, ?SELF_REF} );
    _      -> nothing
  end,
  Participants = get_participants( Data#cross.probe ),
  {keep_state, Data#cross{participants = Participants}};

%% The leader completed the crossing: update participants and wait for 
%%  coordinator message to arrive.
handle_common(cast, crossed, #cross{wait_counter = Wait} = Data) ->
  Participants = get_participants( Data#cross.probe ),
  { keep_state
  , Data#cross{ participants = Participants, wait_counter = Wait + 1 }
  , [ {{timeout, need_election}, ?TIMEOUT_ANSWER, need_election}
    ]
  };

%% If the current leader fails, do a new election.
handle_common(info, {'DOWN', Reference, process, Object, _Info}, Data) ->
  if
    Reference == Data#cross.monitor_ref ->
      internal:a_log( 
        Data#cross.ev_man,
        debug,
        "Intersection coordination",
        lists:concat( ["Leader ", Object, " down! Election needed!"] )
      ),
      cast_vehicles( 
        participants( Data ),
        {election, ?SELF_REF, get_my_id( Data )}
      ),
      { next_state
      , election
      , Data
      , [ {{timeout, election_expired}, ?TIMEOUT_ELECTION, election_expired}
        ]
      };
    true -> keep_state_and_data
  end;

%% I have a mechanical failure. If I'm the leader: pretend to have crossed and
%%  promote the candidate to new leader.
%%  Note: The "fake" crossed notification let other vehicles to start a
%%  neet_election timer, so if they don't receive the coordinator message from
%%  the new leader (actual candiate), they start a new election.
handle_common(cast, mechanical_failure, Data) ->
  if 
    Data#cross.role == leader ->
      internal:a_log( 
        Data#cross.ev_man,
        debug,
        "Intersection coordination",
        lists:concat( 
          [ "Mechanical failure detected. "
          , "Pass the lead to "
          , Data#cross.candidate
          ]
        )
      ),
      cast_vehicles( participants( Data ), crossed ), % pretend to have crossed
      gen_statem:cast( Data#cross.candidate, promotion );
    true -> nothing
  end,
  {stop, normal};

handle_common(cast, Msg, Data) ->
  internal:a_log( 
    Data#cross.ev_man,
    debug,
    "Intersection coordination",
    lists:concat( ["Unexpected message: ", Msg] )
  ),
  keep_state_and_data.



%%% -------------------------- Private Functions --------------------------- %%%

%% Cast all vehicles in the list with the message Msg.
cast_vehicles([AV|Vehicles], Msg) ->
  gen_statem:cast( AV, Msg ),
  cast_vehicles( Vehicles, Msg );
cast_vehicles([], _) -> ok.


%%% References and IDs

%% Get reference to contact the right process located in another node.
get_reference(Node) -> {?MODULE, Node}.

%% Generate nonce to identify the vehicle.
generate_nonce() -> erlang:atom_to_list( node() ).

%% Get current id needed to perform the election algorithm.
get_my_id(Data) -> { Data#cross.wait_counter, Data#cross.nonce }.

%% Greater-than for IDs.
gt_id( {W1, Id1}, {W2, Id2} ) ->
  if
    W1 > W2 -> true;
    W1 == W2, Id1 > Id2 -> true;
    true -> false
  end.


%%% Participants

%% Get all participants at the verge of the intersection from the sensors.
get_participants(Probe) ->
  lists:map(
    fun get_reference/1,
    gen_server:call(Probe, get_participants)
    ).

%% Get participants which are not myself.
participants(Data) ->
  lists:delete( ?SELF_REF, Data#cross.participants ).

%% Get next vehicle in clockwise manner. It is assumed that P is in the list.
clockwise_next([_], _) -> null;
clockwise_next([AV|Vehicles], P) ->
  if
    AV == P -> erlang:hd( Vehicles );
    true    -> clockwise_next( Vehicles ++ [AV], P )
  end;
clockwise_next(_, _) -> null.