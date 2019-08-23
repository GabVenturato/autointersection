-module(environment).
-behavior(gen_server).

-export([start/0, start/1, start_link/0, start_link/1]).
-export([get_participants/0]).
-export([init/1, handle_call/3, handle_cast/2]).

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

%%% Environment utilities:

get_participants() ->
  gen_server:call(?MODULE, participants).

%%% -------------------------- Callback Functions -------------------------- %%%
init([ConfDir]) ->
  InitEnv = #state{ graph = digraph:new() },
  {ok, build_graph(InitEnv, ConfDir)}.

%%% Synchronous requests:

%% get_participants
handle_call(participants, _From, Env) -> 
  {reply, digraph:vertices(Env#state.graph), Env};

%% unexpected message
handle_call(_Msg, _From, Env) -> {reply, ok, Env}.

%%% Asynchronous requests:

handle_cast(_Msg, Env) -> {noreply, Env}.

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
  Env1 = add_nodes(Env, Content),
  add_edges(Env1, Content).

%% 'add_nodes/2'
%% Note that edges are added later in 'add_edges/2' in order to not depend on
%%  the configuration elements order.
%% @param Env : current state record
%% @param Content : list of configuration elements
%% @return : updated state record
add_nodes(Env, [Elem|Content]) ->
  case Elem of
    {edge, _, _} -> add_nodes(Env, Content); % ignore edges
    {V, Node} -> 
      Env1 = case V of
        in  -> Env#state{ ins  = [ Node | Env#state.ins ] };
        out -> Env#state{ outs = [ Node | Env#state.outs ] };
        _   -> Env
      end,
      G = Env1#state.graph,
      digraph:add_vertex( G, Node ),
      Env2 = Env1#state{ graph = G },
      add_nodes(Env2, Content)
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
      add_edges(Env1, Content);
    _ -> add_edges(Env, Content) % ignore nodes
  end;
add_edges(Env, []) -> Env.