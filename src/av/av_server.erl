-module(av_server).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/2, start_link/1]).

-define(EVENT_HANDLER_IDS, [
  {av_communication_event_handler, make_ref()},
  {av_motion_event_handler, make_ref()},
  {av_recognition_event_handler, make_ref()}
]).

%% Event Manager specification
-define(EVENT_MAN_SPEC(Config),
        {event_man,
         {av_event_manager, start_link, [Config]},
         permanent,
         10000,
         worker,
         [av_event_manager]}).

%% Component supervisor specification
-define(COMP_SUP_SPEC(Config),
        {component_sup,
         {component_supervisor, start_link, [Config]},
         permanent,
         10000,
         supervisor,
         [component_supervisor]}).

%% Component specification
-define(COMP_SPEC(Module, Config),
        {Module,
         {Module, start_link, [Config]},
         permanent,
         3000,
         worker,
         [Module]}).
    
-record(internal, {event_maneger,
                   event_handlers,
                   route}).

-record(component, {name,
                    module,
                    pid}).

%%% -------------------------- Interface Functions ------------------------- %%%

start(Route, EnvPid) ->
  gen_server:start({local, av}, ?MODULE, [Route, EnvPid], []).

start_link([Sup, Route, Components, Sensors]) ->
  gen_server:start_link({local, av}, ?MODULE, [Sup, Route, Components, Sensors], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([Sup, Route, Components, Sensors]) ->
  self() ! {startup, Sup, Components, Sensors},
  {ok, #internal{route = Route}}.

%% Start all necessary components and event listeners.
handle_info({startup, Sup, Components, Sensors}, S = #internal{}) ->
  {ok, EvManPid} = supervisor:start_child(Sup, ?EVENT_MAN_SPEC([])),
  {ok, SupPid} = supervisor:start_child(Sup, ?COMP_SUP_SPEC([])),
  add_event_handlers(EvManPid, S),

  self() ! {start_components, SupPid, Components, Sensors},

  {noreply, S#internal{event_maneger = EvManPid}};

%% Start components. Necessary, see page 292 Learn You Some Erlang...
handle_info({start_components, Sup, Components, Sensors}, State) ->

  EvManPid = State#internal.event_maneger,
  Config = {Sup, Sensors, EvManPid},

  FoldingFun = fun(C = #component{}, Acc) -> [start_component(C, Config)|Acc] end,
  StartedComponents = lists:foldl(FoldingFun, [], Components),

  CurrentPosition = current_position(State#internal.route),
  vehicle_sturtup(EvManPid, CurrentPosition, StartedComponents),

  {noreply, State};

handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

%%% Moved message received, clear to move forward in the path.
handle_call({moved}, _From, State = #internal{}) ->

  NewRoute = make_a_step(State#internal.route),
  advance(State#internal.event_maneger, NewRoute),

  {reply, ack, State = #internal{route = NewRoute}};

%%% Deal with position status (free, stop).
handle_call({position_status, Status}, _From, State) ->
  Pid = State#internal.event_maneger,
  case Status of
    free -> 
      advance(Pid, State#internal.route);
    stop -> 
      resolve(Pid, stop);
    _ -> 
      io:format("Cannot handle such Status! ~p~n", [Status])
  end,
  {reply, ack, State}.

handle_cast(_, State) -> {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% -------------------------- Private Functions --------------------------- %%%

%% Function to start a component C, returns the started component.
start_component(C = #component{module = Module}, {Sup, Sensors, EvManPid}) ->
  {ok, Pid} = supervisor:start_child(Sup, ?COMP_SPEC(Module, {Sensors, EvManPid})),
  C = #component{pid = Pid}.

%% Function necessary to 'startup the vehicle', to start the whole vehicle logic.
vehicle_sturtup(Pid, Pos, Components) ->
  {ok, Pid} = gen_event:add_handler(Pid,
                        av_server_event_handler, 
                        [Components]),
  sync_notify(Pid, {startup, Pos}).

%% Adds a list of event handlers to the event manager.
add_event_handlers(Pid, S = #internal{}) ->
  FoldingFun = fun(HandlerId, Acc) -> [gen_event:add_handler(Pid, HandlerId, [self()])|Acc] end,
  lists:foldl(FoldingFun, [], ?EVENT_HANDLER_IDS),
  S#internal{event_handlers = ?EVENT_HANDLER_IDS}.

%% Remove all event handlers from the AV server.
remove_event_handlers(Pid, S = #internal{}) ->
  HandlerIds = S#internal.event_handlers,
  FoldingFun = fun(HandlerId, Acc) -> [gen_event:delete_handler(Pid, HandlerId)|Acc] end,
  lists:foldl(FoldingFun, [], HandlerIds),
  S#internal{event_handlers = []}.

%% The vehicle is ready to advance to the next position.
advance(Pid, Route) ->
  Next = next_position(Route),
  case Next of
    {ok, Position} -> sync_notify(Pid, {move, Position});
    _ -> 
      sync_notify(Pid, {route_completed}),
      hd(Route) % Temp, returns current node to avoid errors.
  end,
  sync_notify(Pid, {move, Next}).

%% Resolve a node situation (free, stop), only stop needs to be resolved.
resolve(Pid, stop) ->
  sync_notify(Pid, {handle_status, stop}).

%% Make a step in the route.
make_a_step(Route) ->
  tl(Route).

%% Get next position, if available, otherwise return an error because the route
%% is completed.
next_position([]) -> {error, route_completed};
next_position([_|Tail]) ->
  hd(Tail).

%% Return the current position in the route.
current_position(Route) -> hd(Route).

%% Generic synchronous event notification.
sync_notify(Pid, Msg) ->
  gen_event:sync_notify(Pid, Msg).

