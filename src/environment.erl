-module(environment).
-behavior(gen_server).

-export([start/0, start/1, start_link/0, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([ enqueue_vehicle/2, dequeue_vehicle/1, is_position_free/1
        , first_in_queue/2, update_position/3, get_participants/0
        , get_crossing_participants/0, get_position_status/1, terminate/2
        ]).

-record(state,
  { ins = []
  , outs = []
  , graph
  }).

-define(CONF_DIR, "conf/").
-define(INTERSECTION_CONF_FILE, "intersection.conf").

%%% -------------------------- Interface Functions ------------------------- %%%

%%% Functions 'start' and 'start_link' can be called both without parameters, or 
%%% specifying the path to the directory containing the config files.

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [?CONF_DIR], []).

%% @param ConfDir : path to the directory containing the config files
start(ConfDir) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [ConfDir], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @param ConfDir : path to the directory containing the config files
start_link(ConfDir) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfDir], []).

stop() ->
  gen_server:cast(?MODULE, stop).


%%% Environment utilities:

enqueue_vehicle(Vehicle, Pos) ->
  gen_server:cast(?MODULE, {enqueue_vehicle, Vehicle, Pos}).

dequeue_vehicle(Pos) ->
  gen_server:cast(?MODULE, {dequeue_vehicle, Pos}).

is_position_free(Pos) ->
  gen_server:call(?MODULE, {is_position_free, Pos}).

first_in_queue(Vehicle, Pos) ->
  gen_server:call(?MODULE, {first_in_queue, Vehicle, Pos}).

update_position(Vehicle, OldPos, NewPos) ->
  gen_server:call(?MODULE, {update_position, Vehicle, OldPos, NewPos}).

get_position_status(Pos) ->
  gen_server:call(?MODULE, {position_status, Pos}).

get_participants() ->
  gen_server:call(?MODULE, get_participants).

get_crossing_participants() ->
  gen_server:call(?MODULE, get_crossing_participants).

%%% -------------------------- Callback Functions -------------------------- %%%
init([ConfDir]) ->
  InitEnv = #state{ graph = digraph:new() },
  {ok, build_graph(InitEnv, ConfDir)}.


%%% Synchronous requests:

handle_call({is_position_free, Pos}, _From, Env) ->
  case digraph:vertex( Env#state.graph, Pos ) of
    {Pos, []} -> {reply, true, Env};
    {Pos, _ } -> {reply, false, Env};
    _ ->
      io:format( "Unexpected position: ~p~n", [Pos] ),
      {reply, false, Env}
  end;

handle_call({first_in_queue, Vehicle, Pos}, _From, Env) ->
  case digraph:vertex( Env#state.graph, Pos ) of
    {Pos, [Vehicle|_]} -> {reply, true, Env};
    {Pos, _ }          -> {reply, false, Env};
    _ ->
      io:format( "Unexpected position: ~p~n", [Pos] ),
      {reply, false, Env}
  end;

%% TODO: should be performed more checks? like if the two position are connected
%%   by an edge.
handle_call({update_position, Vehicle, OldPos, NewPos}, _From, Env) ->
  case digraph:vertex( Env#state.graph, OldPos ) of
    {OldPos, [Vehicle|_]} -> 
      dequeue_vehicle( OldPos ),
      enqueue_vehicle( Vehicle, NewPos ),
      {reply, ok, Env};
    {OldPos, _ } ->
      io:format( "Vehicle ~p not found at position ~p~n", [Vehicle, OldPos] ),
      {reply, error, Env};
    _ ->
      io:format( "Unexpected position: ~p~n", [OldPos] ),
      {reply, error, Env}
  end;

handle_call({position_status, Pos}, _From, Env) ->
  case lists:member(Pos, Env#state.ins) of
    true  -> {reply, free, Env};
    false -> {reply, stop, Env}
  end;

%% get_participants
handle_call(get_participants, _From, Env) ->
  Participants = get_participants_list( Env, Env#state.ins ),
  {reply, Participants, Env};

%% get_crossing_participants
handle_call(get_crossing_participants, _From, Env) ->
  Pos = lists:subtract( digraph:vertices( Env#state.graph ), Env#state.ins ),
  Participants = get_participants_list( Env,  Pos ),
  {reply, Participants, Env};

%% unexpected message
handle_call(Msg, _From, Env) -> {reply, {unexpected_message, Msg}, Env}.


%%% Asynchronous requests:

handle_cast({enqueue_vehicle, Vehicle, Pos}, Env) ->
  case digraph:vertex( Env#state.graph, Pos ) of
    {Pos, Queue} ->
      digraph:add_vertex( Env#state.graph, Pos, Queue ++ [Vehicle]),
      io:format(
        "Queue at ~p is: ~p~n",
        [ Pos, digraph:vertex( Env#state.graph, Pos ) ] );
    _ ->
      io:format( "Unexpected position: ~p~n", [Pos] )
  end,
  {noreply, Env};

handle_cast({dequeue_vehicle, Pos}, Env) ->
  case digraph:vertex( Env#state.graph, Pos ) of
    {Pos, [_|Queue]} ->
      digraph:add_vertex( Env#state.graph, Pos, Queue),
      io:format(
        "Queue at ~p is: ~p~n",
        [ Pos, digraph:vertex( Env#state.graph, Pos ) ] );
    {Pos, _} ->
      io:format( "Can't dequeue on empty position ~p~n", [Pos] );
    _ ->
      io:format( "Unexpected position: ~p~n", [Pos] )
  end,
  {noreply, Env};

handle_cast(stop, Env) -> {stop, normal, Env};

handle_cast(_Msg, Env) -> {noreply, Env}.


%%% Handle info:

handle_info(Msg, Env) ->
  io:format( "Unexpected message: ~p~n", [Msg] ),
  {noreply, Env}.


%%% Code change:

code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behavior, but will not be 
  %% used.
  {ok, State}.


%%% Stop the environment process

terminate(normal, _Env) -> ok.
% terminate({error, Reason}, _Env) -> {error, Reason}.

%%% -------------------------- Private Functions --------------------------- %%%

%%% Build intersection graph from the configuration file:

%% 'build_graph/2'
%% @param Env : current state record
%% @param Conf : path to the configuration directory
%% @return : new state record with 'graph', 'ins', and 'outs' fileds 
%%  correctly filled.
build_graph(Env, ConfDir) ->
  IntersectionConfFile = filename:absname( ?INTERSECTION_CONF_FILE , ConfDir ),
  {ok, Content} = file:consult( IntersectionConfFile ),
  Env1 = add_nodes( Env, Content ),
  add_edges( Env1, Content ).

%% 'add_nodes/2'
%% Note that edges are added later in 'add_edges/2' in order to not depend on
%%  the configuration elements order.
%% @param Env : current state record
%% @param Content : list of configuration elements
%% @return : updated state record
add_nodes(Env, [Elem|Content]) ->
  case Elem of
    {edge, _, _} -> add_nodes( Env, Content ); % ignore edges
    {V, Node} -> 
      Env1 = case V of
        in  -> Env#state{ ins  = Env#state.ins ++ [Node] };
        out -> Env#state{ outs = Env#state.outs ++ [Node] };
        _   -> Env
      end,
      G = Env1#state.graph,
      digraph:add_vertex( G, Node ),
      Env2 = Env1#state{ graph = G },
      add_nodes( Env2, Content )
  end;
add_nodes(Env, []) -> Env.

%% 'add_edges/2'
%% @param Env : current state record
%% @param Content : list of configuration elements
%% @return : updated state record
add_edges(Env, [Elem|Content]) ->
  case Elem of
    {edge, Node1, Node2} -> 
      G = Env#state.graph,
      digraph:add_edge( G, Node1, Node2 ),
      Env1 = Env#state{ graph = G },
      add_edges( Env1, Content );
    _ -> add_edges( Env, Content ) % ignore nodes
  end;
add_edges(Env, []) -> Env.


%%% Get first vehicles from each queue in input

get_participants_list( Env, [Pos|Positions] ) ->
  case digraph:vertex( Env#state.graph, Pos ) of
    {Pos, [Vehicle|_]} -> [ Vehicle | get_participants_list( Env, Positions ) ];
    _                  -> get_participants_list( Env, Positions )
  end;
get_participants_list( _, [] ) -> [].