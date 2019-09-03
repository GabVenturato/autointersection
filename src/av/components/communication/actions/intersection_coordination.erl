-module(intersection_coordination).
-behavior(gen_statem).

-include("../../../../../include/event.hrl").

-export([start/2, start_link/2, callback_mode/0]).
-export([print_state/0]).
-export([init/1, terminate/3, handle_common/3]).
-export([ready/3, election/3, crossing/3]).

-record( cross,
  { env               % environment reference (i.e. Pid / info to contact it)
  , ev_man            % event manager reference
  , nonce             % note: ID = (nonce, wait_counter)
  , wait_counter = 0  % how many vehicles crossed before me
  , participants      % list that include myself
  , role = simple     % possible values: simple | leader
  , leader = null     % leader reference
  , candidate = null  % candidate reference
  }).

-define( TIMEOUT_ELECTION, 2 * 1000 ). % milliseconds
-define( TIMEOUT_ANSWER, ?TIMEOUT_ELECTION * 2 ).
-define( SELF_REF, {?MODULE, node()} ).

-define( EVENT_HANDLER_ID, {ic_event_handler, make_ref()} ).

-define( HANDLE_COMMON, ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D) ).

%%% -------------------------- Interface Functions ------------------------- %%%

start(Env, EvMan) ->
  gen_statem:start({local, ?MODULE}, ?MODULE, [Env, EvMan], []).

start_link(Env, EvMan) ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [Env, EvMan], []).


callback_mode() ->
  state_functions.

%% Print current data
print_state() ->
  gen_statem:cast(?MODULE, print_state).

%%% -------------------------- Callback Functions -------------------------- %%%

init([Env, EvMan]) ->
  io:format("INIT CALLED~n"),
  process_flag(trap_exit, true),
  gen_event:add_handler(EvMan, ?EVENT_HANDLER_ID, [self()]),

  Data = #cross
    { env = Env
    , ev_man = EvMan
    , nonce = generate_nonce()
    , participants = get_participants( Env )
    },
  
  Participants = participants( Data ),
  AllParticipants = Participants ++ lists:map(
    fun get_reference/1,
    gen_server:call(Env, get_crossing_participants)
  ),
  io:format( "Participants: ~p~n", [Participants] ),
  io:format( "AllParticipants: ~p~n", [AllParticipants] ),
  
  cast_vehicles( AllParticipants, {whos_leader, ?SELF_REF} ),
  { ok
  , ready
  , Data
  , [ {{timeout, need_election}, ?TIMEOUT_ELECTION, need_election}
    ]
  }.

terminate(Reason, _State, Data) ->
  gen_event:delete_handler( Data#cross.ev_man, ?EVENT_HANDLER_ID, [] ),
  {ok, Reason}.


%%% STATE: READY
%%%  AV ready to cross but it is not the leader, waiting its turn.

%% Timout need_election expired -> an election is needed
ready({timeout, need_election}, need_election, Data) ->
  io:format( "Timeout expired, election needed.~n" ),
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

%% New leader received (when a candidate becomes the new leader)
ready(cast, {leader, {_, Node } = L}, Data) ->
  io:format( "Found leader: ~p~n", [L] ),
  erlang:monitor_node( Node, true ),
  { keep_state
  , Data#cross{ leader = L }
  , [ {{timeout, need_election}, infinity, undefined}
    ]
  };

%% Received a request to perform an election, postpone it to "election" state
ready(cast, {election, _From, _Id}, Data) ->
  { next_state
  , election
  , Data
  , [ postpone
    , {{timeout, need_election}, infinity, undefined}
    ]
  };

?HANDLE_COMMON.


%%% STATE: ELECTION
%%%  Participants need to find a leader which will starts to cross

%% Election message was sent, but no one answered, so I am the leader.
election({timeout, election_expired}, election_expired, Data) ->
  io:format( "No one answered my election message. I'm the leader.~n" ),
  cast_vehicles( participants( Data ), {coordinator, ?SELF_REF} ),
  gen_event:notify(
    Data#cross.ev_man, 
    #event{type = notification, name = position_type, content = normal}
  ),
  { next_state
  , crossing
  , Data#cross
      { role = leader
      , candidate = clockwise_next( Data#cross.participants, ?SELF_REF )
      }
  };

%% Expected to receive coordinator message but none arrived, so do new election
election({timeout, answer_expired}, answer_expired, Data) ->
  io:format( "Answer expired, election needed.~n" ),
  cast_vehicles(
    participants( Data ),
    {election, ?SELF_REF, get_my_id( Data )}
  ),
  { keep_state_and_data
  , [ {{timeout, election_expired}, ?TIMEOUT_ELECTION, election_expired}
    ]
  };

%% Election message received: forward the election only if myID > receivedID
election(cast, {election, From, Id}, Data) ->
  io:format( "Election message received." ),
  MyId = get_my_id( Data ),
  Gt = gt_id( MyId, Id ),
  case Gt of
    true -> 
      io:format( " Forward election.~n" ),
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
      io:format( " Not forward election.~n" ),
      { keep_state_and_data
      , [ {{timeout, answer_expired}, ?TIMEOUT_ANSWER, answer_expired}
        ]
      }
  end;

%% Answer message received: I am not the one with the greatest ID
election(cast, answer, _Data) ->
  io:format( "Received answer. Wait for coordinator message.~n" ),
  { keep_state_and_data
  , [ {{timeout, election_expired}, infinity, undefined}
    , {{timeout, answer_expired}, ?TIMEOUT_ANSWER, answer_expired}
    ]
  };

%% Coordinator message received. Leader is elected, monitor it and conclude 
%%  the election algorithm
election(cast, {coordinator, {_, Node} = L}, Data) ->
  io:format( "Found coordinator: ~p~n", [L] ),
  erlang:monitor_node( Node, false ), % remove old monitor if exists
  erlang:monitor_node( Node, true ),  % start a new monitor
  { next_state
  , ready
  , Data#cross{ leader = L }
  , [ {{timeout, answer_expired}, infinity, undefined}
    , {{timeout, election_expired}, infinity, undefined}
    ]
  };

?HANDLE_COMMON.

%%% STATE: CROSSING
crossing(cast, crossing_complete, Data) ->
  io:format( 
    "Crossing complete! Pass the lead to ~p~n", 
    [Data#cross.candidate] 
  ),
  {stop, normal};

crossing(cast, {whos_leader, From}, Data) ->
  gen_statem:cast( From, {leader, ?SELF_REF} ),
  Participants = get_participants( Data#cross.env ),
  {keep_state, Data#cross{participants = Participants}};

crossing(cast, Msg, _Data) ->
  io:format( "Ignoring message: ~p~n", [Msg] ),
  keep_state.

%%% STATE: CRASH


%%% HANDLE COMMON

%% New vehicle arrived at the intersection and asked who is the new leader:
%%  answere only if I am the leader
handle_common(cast, {whos_leader, From}, Data) ->
  case Data#cross.role of
    leader -> gen_statem:cast( From, {leader, ?SELF_REF} );
    _      -> nothing
  end,
  Participants = get_participants( Data#cross.env ),
  {keep_state, Data#cross{participants = Participants}};

%% The leader completed the crossing
handle_common(cast, crossed, #cross{wait_counter = Wait} = Data) ->
  {keep_state, Data#cross{wait_counter = Wait + 1}};

%% Print current data
handle_common(cast, print_state, Data) ->
  io:format("DATA RECORD
    env: ~p
    id: ~p
    participants: ~p
    role: ~p
    leader: ~p
    candidate: ~p
    wait_counter: ~p~n",
    [ Data#cross.env, Data#cross.nonce, Data#cross.participants
    , Data#cross.role, Data#cross.leader, Data#cross.candidate
    , Data#cross.wait_counter]),
  keep_state_and_data;

% Only the leader is  monitored, so if received nodedown is because it failed
handle_common(info, {nodedown, Node}, _Data) ->
  io:format( "Leader down: ~p~n", [Node] ),
  keep_state_and_data;

handle_common(cast, Msg, _Data) ->
  io:format( "Unexpected message: ~p~n", [Msg] ),
  keep_state_and_data.



%%% -------------------------- Private Functions --------------------------- %%%

%% Cast all vehicles in the list with the message Msg
cast_vehicles([AV|Vehicles], Msg) ->
  gen_statem:cast( AV, Msg ),
  cast_vehicles( Vehicles, Msg );
cast_vehicles([], _) -> ok.


%%% References and IDs

%% Get reference to contact the right process located in another node
get_reference(Node) -> {?MODULE, Node}.

%% Generate nonce to identify the vehicle
generate_nonce() ->
  Node = erlang:atom_to_list( node() ),
  Pid = erlang:pid_to_list( self() ),
  crypto:hash( md5, Node ++ Pid ).

%% Get current id needed to perform the election algorithm
get_my_id(Data) -> { Data#cross.wait_counter, Data#cross.nonce }.

%% Greater-than for IDs
gt_id( {W1, Id1}, {W2, Id2} ) ->
  if
    W1 > W2 -> true;
    W1 == W2, Id1 > Id2 -> true;
    true -> false
  end.


%%% Participants

%% Get all participants at the verge of the intersection from the environment
get_participants(Env) ->
  lists:map(
    fun get_reference/1,
    gen_server:call(Env, get_participants)
    ).

%% Get participants which are not myself
participants(Data) ->
  lists:delete( ?SELF_REF, Data#cross.participants ).

%% Get next vehicle in clockwise manner. It is assumed that P is in the list
clockwise_next([_], _) -> null;
clockwise_next([AV|Vehicles], P) ->
  if
    AV == P -> erlang:hd( Vehicles );
    true    -> clockwise_next( Vehicles ++ [AV], P )
  end;
clockwise_next(_, _) -> null.