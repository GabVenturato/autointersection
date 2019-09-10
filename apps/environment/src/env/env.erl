-module(env).
-behavior(gen_server).

-export([start/0, start/1, start_link/0, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([ is_position_free/1, update_position/3, get_participants/0
        , get_crossing_participants/0, get_position_type/1, get_route/2
        , get_start_positions/0, get_finish_positions/0, get_entrance_number/0
        , terminate/2
        ]).

-record(state,
  { ins = []
  , outs = []
  , internal = []
  , start = []
  , finish = []
  , graph
  , waiting_list
  , vehicle_list
  , logger
  }).

-record(vertex_info,
  { vehicle
  , type
  }).

-include( "event.hrl" ).

-define( CONF_DIR, "apps/environment/conf/" ).
-define( INTERSECTION_CONF_FILE, "intersection.conf" ).

-define( TOW_TRUCK_TIME, 30000 ).

%%% -------------------------- Interface Functions ------------------------- %%%

%%% Functions 'start' and 'start_link' can be called both without parameters, or 
%%% specifying the path to the directory containing the config files.

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [?CONF_DIR], []).

%% @param ConfDir : path to the directory containing the config files
start(ConfDir) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [ConfDir], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [?CONF_DIR], []).

%% @param ConfDir : path to the directory containing the config files
start_link(ConfDir) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [ConfDir], []).

stop() ->
  gen_server:cast(?MODULE, stop).


%%% Environment utilities:

is_position_free(Pos) ->
  gen_server:call(?MODULE, {is_position_free, Pos}).

update_position(Vehicle, OldPos, NewEnv) ->
  gen_server:call(?MODULE, {update_position, {Vehicle, OldPos, NewEnv}}).

get_position_type(Pos) ->
  gen_server:call(?MODULE, {position_type, Pos}).

get_route(Pos1, Pos2) ->
  gen_server:call(?MODULE, {get_route, {Pos1, Pos2}}).

get_participants() ->
  gen_server:call(?MODULE, get_participants).

get_crossing_participants() ->
  gen_server:call(?MODULE, get_crossing_participants).

get_start_positions() ->
  gen_server:call(?MODULE, get_start_positions).

get_finish_positions() ->
  gen_server:call(?MODULE, get_finish_positions).

get_entrance_number() ->
  gen_server:call(?MODULE, get_entrance_number).

%%% -------------------------- Callback Functions -------------------------- %%%
init([ConfDir]) ->
  InitEnv = #state{ graph = digraph:new(),
                    waiting_list = dict:new(),
                    vehicle_list = dict:new() },
  NewEnv = build_graph(InitEnv, ConfDir),
  {ok, Logger} = env_logger:start_link(),
  io:format( "Env started! ~n"),
  {ok, NewEnv#state{logger = Logger}}.


%%% Synchronous requests:

handle_call({is_position_free, Pos}, _From, Env) ->
  {reply, is_position_free(Pos, Env), Env};

handle_call({update_position, {Vehicle, OldPos, NewPos}}, _From, Env) ->
  NewEnv = release(OldPos, Env),
  NewestEnv = add(Vehicle, NewPos, NewEnv),
  Msg = lists:concat(
    [ "Vehicle "
    , Vehicle
    , " moved from position "
    , OldPos
    , " to "
    , NewPos
    , "."]
  ),
  gen_server:call(Env#state.logger, {log, "Vehicle position updated", Msg}),
  {reply, ok, NewestEnv};

handle_call({vehicle_at, Pos}, _From, Env) ->
  Result = case digraph:vertex(Env#state.graph, Pos) of
    {_, #vertex_info{vehicle = undefined}} -> {nothing};
    {_V, #vertex_info{vehicle = Pid}} -> {vehicle, Pid}
  end,
  {reply, Result, Env};

handle_call({position_type, Pos}, _From, Env) ->
  ResultType = case digraph:vertex(Env#state.graph, Pos) of
    {_, []} -> undefined;
    {_, #vertex_info{type = Type}} -> Type;
    _ -> external
  end,
  {reply, ResultType, Env};

handle_call({get_route, {Pos1, Pos2}}, _From, Env) ->
  {reply, get_route(Env, Pos1, Pos2), Env};

%% get_participants
handle_call(get_participants, _From, Env) ->
  Participants = get_participants_list( Env, Env#state.ins ),
  {reply, Participants, Env};

%% get_crossing_participants
handle_call(get_crossing_participants, _From, Env) ->
  Pos = Env#state.internal ++ Env#state.outs,
  Participants = get_participants_list( Env,  Pos ),
  {reply, Participants, Env};

%% get_start_positions
handle_call(get_start_positions, _From, Env) ->
  {reply, Env#state.start, Env};

%% get_finish_positions
handle_call(get_finish_positions, _From, Env) ->
  {reply, Env#state.finish, Env};

%% get_entrance_number
handle_call(get_entrance_number, _From, Env) ->
  {reply, erlang:length( Env#state.ins ), Env};

%% unexpected message
handle_call(Msg, _From, Env) -> {reply, {unexpected_message, Msg}, Env}.


%%% Asynchronous requests:

handle_cast({notify_when_free, {WaitingPid, Position, Ref}}, Env) ->
  NewWaitingList = case is_position_free(Position, Env) of
    true -> 
      notify_free_position({WaitingPid, Ref}),
      Env#state.waiting_list;
    false ->
      add_waiting_vehicle(Position, {WaitingPid, Ref}, Env#state.waiting_list)
  end,
  {noreply, Env#state{waiting_list = NewWaitingList}};

handle_cast({vehicle_down, Vehicle}, Env) ->
  Position = case dict:find(Vehicle, Env#state.vehicle_list) of
    {ok, [Pos]} -> Pos;
    error -> []
  end,
  erlang:send_after(?TOW_TRUCK_TIME, self(), {cleanup_position, Position}),
  {noreply, Env};

handle_cast(stop, Env) -> {stop, normal, Env};

handle_cast(_Msg, Env) -> {noreply, Env}.


%%% Handle info:

handle_info({cleanup_position, Pos}, Env) ->
  NewEnv = release(Pos, Env),
  {noreply, NewEnv};

handle_info(Msg, Env) ->
  io:format( "Unexpected message: ~p~n", [Msg] ),
  {noreply, Env}.


%%% Code change:

code_change(_OldVsn, State, _Extra) ->
  %% No change planned. The function is there for the behavior, but will not be 
  %% used.
  {ok, State}.


%%% Stop the environment process

terminate(Reason, _Env) -> {ok, Reason}.

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
  NewEnv1 = add_nodes( Env, Content ),
  NewEnv2 = add_edges( NewEnv1, Content ),
  NewEnv2.

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
        intersection_entrance ->
          Env#state{ ins  = Env#state.ins ++ [Node] };
        intersection_exit ->
          Env#state{ outs = Env#state.outs ++ [Node] };
        intersection_internal -> 
          Env#state{ internal = Env#state.internal ++ [Node] };
        start ->
          Env#state{ start = Env#state.start ++ [Node] };
        finish ->
          Env#state{ finish = Env#state.finish ++ [Node] };
        _ -> Env
      end,
      G = Env1#state.graph,
      digraph:add_vertex( G, Node, #vertex_info{type = V} ),
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
    {Pos, #vertex_info{vehicle = undefined}} ->
      get_participants_list( Env, Positions );
    {Pos, #vertex_info{vehicle = Vehicle}} ->
      [ Vehicle | get_participants_list( Env, Positions ) ];
    _ ->
      get_participants_list( Env, Positions )
  end;
get_participants_list( _, [] ) -> [].

%% Add Pid in Pos.
add(_, [], Env) ->
  Env;

add(Pid, Pos, Env) ->
  case digraph:vertex(Env#state.graph, Pos) of
    {_, #vertex_info{vehicle = undefined, type = Type}} -> 
        digraph:add_vertex(
          Env#state.graph,
          Pos,
          #vertex_info{vehicle = Pid, type = Type}
        ),
        VehicleList1 = remove_vehicle(Pid, Env#state.vehicle_list),
        VehicleList2 = dict:append(Pid, Pos, VehicleList1),
        Env#state{vehicle_list = VehicleList2};
    {_, #vertex_info{}} -> 
      io:format("A vehicle is already in that position: ~p~n", [Pos]),
      Env;
    _ -> 
      io:format("Unknown position: ~p~n", [Pos]),
      Env
  end.
  

release(Pos, Env) ->
  case digraph:vertex(Env#state.graph, Pos) of
    {V, #vertex_info{vehicle = Pid, type = Type}} -> 
      digraph:add_vertex(Env#state.graph, V, #vertex_info{type = Type}),
      NewVehicleList = remove_vehicle(Pid, Env#state.vehicle_list),
      NewWaitingList = release_waiting_vehicle(Pos, Env#state.waiting_list),
      Env#state{waiting_list = NewWaitingList, vehicle_list = NewVehicleList};
    _ -> Env
  end.

%% Remove vehicle with Pid from the graph and return its position.
remove_vehicle(Pid, VehicleList) ->
  dict:erase(Pid, VehicleList).

add_waiting_vehicle(Position, {WaitingPid , Ref}, WaitingList) ->
  %% Erase in case an old reference is present.
  NewWaitingList = dict:erase(Position, WaitingList),
  dict:append(Position, {WaitingPid, Ref}, NewWaitingList).

%% Take the element with key 'Position' from the list. Returns the same list
%% if the key is not present in the list.
release_waiting_vehicle(Position, Waiting_list) ->
  case dict:take(Position, Waiting_list) of
    {[Value], NewList} ->
      notify_free_position(Value),
      NewList;
    error -> Waiting_list
  end.

notify_free_position(Value) ->
  {Requester, Ref} = Value,
  gen_event:notify(
    Requester, 
    #event{type = notification, name = clear_to_move, content = Ref}
  ).

is_position_free(Pos, Env) ->
  case digraph:vertex( Env#state.graph, Pos ) of
    {Pos, []} -> false;
    {Pos, #vertex_info{vehicle = undefined}} -> true;
    {Pos, #vertex_info{vehicle = _}} -> false;
    {Pos, _} -> false;
    _ ->
      io:format( "Unexpected position: ~p~n", [Pos] ),
      false
  end.

get_route(Env, Pos1, Pos2) ->
  digraph:get_short_path(Env#state.graph, Pos1, Pos2).