-module(av_intersection_coordination).
-behavior(gen_statem).

-export([start/2, start_link/2, callback_mode/0]).
-export([init/1, handle_common/3]).
-export([ready/3, election/3]).

-record( state,
  { env
  , ev_man
  , id
  , wait_counter = 0
  , participants
  , role = simple     % possible values: simple | leader
  , leader = null
  , candidate = null
  }).

-define( TIMEOUT_ELECTION, 3000 ).
-define( SELF_REF, {?MODULE, node()} ).

-define(HANDLE_COMMON, ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, D)).

%%% -------------------------- Interface Functions ------------------------- %%%

start(Env, EvMan) ->
  gen_statem:start({local, ?MODULE}, ?MODULE, [Env, EvMan], []).

start_link(Env, EvMan) ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [Env, EvMan], []).


callback_mode() ->
  state_functions.

%%% -------------------------- Callback Functions -------------------------- %%%

init([Env, EvMan]) ->
  Participants = get_participants( Env ),
  AllParticipants = Participants ++ lists:map(
    fun get_reference/1,
    gen_server:call(Env, get_crossing_participants)
  ),
  io:format( "Participants: ~p~n", [Participants] ),
  io:format( "AllParticipants: ~p~n", [AllParticipants] ),
  
  State = #state
    { env = Env
    , ev_man = EvMan
    , id = generate_id()
    , participants = Participants
    },
  
  cast_vehicles( AllParticipants, {whos_leader, ?SELF_REF} ),
  { ok
  , ready
  , State
  , [ {{timeout, need_election}, ?TIMEOUT_ELECTION, need_election}
    ]
  }.


%%% STATE: READY
%%%  AV ready to cross but it is not the leader, waiting its turn.

ready({timeout, need_election}, need_election, State) ->
  io:format( "Timeout expired, election needed.~n" ),
  cast_vehicles( 
    State#state.participants,
    {election, ?SELF_REF, get_my_id( State )}
  ),
  { next_state
  , election
  , State
  , [ {{timeout, election_expired}, ?TIMEOUT_ELECTION, election_expired}
    ]
  };

ready(cast, {leader, L}, State) ->
  io:format( "Found leader: ~p~n", [L] ),
  { keep_state
  , State#state{ leader = L }
  , [ {{timeout, need_election}, infinity, undefined}
    ]
  };

?HANDLE_COMMON.


%%% STATE: ELECTION
%%%  Participants need to find a leader which will starts to cross

%% Election message was sent, but no one answered, so I am the leader.
election({timeout, election_expired}, election_expired, State) ->
  io:format( "No one answered my election message. I'm the leader.~n" ),
  cast_vehicles( State#state.participants, {coordinator, ?SELF_REF} ),
  { next_state
  , ready
  , State#state
      { role = leader
      , candidate = clockwise_next( State#state.participants, ?SELF_REF )
      }
  };

%% Expected to receive coordinator message but none arrived, so do new election
election({timeout, answer_expired}, answer_expired, State) ->
  io:format( "Answer expired, election needed.~n" ),
  cast_vehicles(
    State#state.participants,
    {election, ?SELF_REF, get_my_id( State )}
  ),
  { keep_state_and_data
  , [ {{timeout, election_expired}, ?TIMEOUT_ELECTION, election_expired}
    ]
  };

election(cast, {election, From, Id}, State) ->
  MyId = get_my_id( State ),
  Gt = gt_id( MyId, Id ),
  if Gt -> 
    io:format( "Election message received, forward election.~n" ),
    gen_statem:cast( From, answer ),
    cast_vehicles( 
      State#state.participants,
      {election, ?SELF_REF, MyId}
    )
  end,
  { keep_state_and_data
  , [ {{timeout, election_expired}, ?TIMEOUT_ELECTION, election_expired}
    ]
  };

election(cast, answer, _State) ->
  io:format( "Received answer. Wait for coordinator message." ),
  { keep_state_and_data
  , [ {{timeout, election_expired}, infinity, undefined}
    , {{timeout, answer_expired}, ?TIMEOUT_ELECTION, answer_expired}
    ]
  };

election(cast, {coordinator, {_, Node} = L}, State) ->
  io:format( "Found leader: ~p~n", [L] ),
  erlang:monitor_node( Node, true ),
  { next_state
  , ready
  , State#state{ leader = L }
  , [ {{timeout, answer_expired}, infinity, undefined}
    ]
  };

?HANDLE_COMMON.


%%% HANDLE COMMON

handle_common(cast, {whos_leader, From}, State) ->
  if 
    State#state.role == leader -> 
      gen_statem:cast( From, {leader, ?SELF_REF} )
  end,
  Participants = get_participants( State#state.env ),
  {keep_state, State#state{participants = Participants}};

handle_common(cast, crossed, #state{wait_counter = Wait} = State) ->
  {keep_state, State#state{wait_counter = Wait + 1}};

handle_common(cast, print_state, State) ->
  io:format("STATE RECORD
    env: ~p
    id: ~p
    participants: ~p
    leader: ~p
    role: ~p
    wait_counter: ~p~n",
    [ State#state.env, State#state.id, State#state.participants
    , State#state.leader, State#state.role, State#state.wait_counter]),
  keep_state_and_data.


%%% -------------------------- Private Functions --------------------------- %%%

%% Generate unique identifier
generate_id() ->
  Node = erlang:atom_to_list( node() ),
  Pid = erlang:pid_to_list( self() ),
  crypto:hash( md5, Node ++ Pid ).

%% Monitor a list of vehicles
% monitor_vehicles([AV|Vehicles]) ->
%   erlang:monitor( process, AV ),
%   monitor_vehicles( Vehicles );
% monitor_vehicles([]) -> ok.

cast_vehicles([AV|Vehicles], Msg) ->
  gen_statem:cast( AV, Msg ),
  cast_vehicles( Vehicles, Msg );
cast_vehicles([], _) -> ok.

%% Assumed P is in the first argument list
clockwise_next([AV|Vehicles], P) ->
  if
    AV == P -> erlang:hd( Vehicles );
    true    -> clockwise_next( Vehicles ++ AV, P )
  end;
clockwise_next(_, _) -> null.

%% Get reference to contact the right process located in another node
get_reference(Node) -> {?MODULE, Node}.

%% Get current id needed to perform the election algorithm
get_my_id(State) -> { State#state.wait_counter, State#state.id }.

gt_id( {W1, Id1}, {W2, Id2} ) ->
  if
    W1 > W2 -> true;
    W1 == W2, Id1 > Id2 -> true;
    true -> false
  end.

get_participants(Env) ->
  lists:map(
    fun get_reference/1,
    lists:delete( node(), gen_server:call(Env, get_participants) )
    ).