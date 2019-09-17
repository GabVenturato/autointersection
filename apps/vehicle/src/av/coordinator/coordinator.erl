%%% The coordinator represents the logic and reasoning of the system, 
%%% coordinating the various components in order to achieve the vehicle’s goals.
%%% The coordinator listens for internal events of interest by registering 
%%% event listeners on the event manager (just like components do).
%%% Specific internal events trigger the coordinator’s actions which, in turn, 
%%% produce new internal events addressed to specific components. 
%%% This behavior of recognizing specific events and addressing specific 
%%% components is what drives the vehicle towards its goals.

-module(coordinator).
-behavior(gen_server).

% Interface functions.
-export([startup/0, initialize/2, set_testing_environment/1]).
-export([start/1, start_link/1, start/3, start_link/3]).
-export([cause_mechanical_failure/0]).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%$ Event Handlers (listeners) registered by the coordinator.
-define(EVENT_HANDLER_IDS, [
  {communication_event_handler, make_ref()},
  {motion_event_handler, make_ref()},
  {recognition_event_handler, make_ref()}
]).

-define(ENV_HANDLER, {env_event_handler, make_ref()}).

%% Event Manager specification
-define(EVENT_MAN_SPEC(Args),
        {event_man,
         {gen_event, start_link, Args},
         temporary,
         10000,
         worker,
         [gen_event]}).

%% Component supervisor specification
-define(COMP_SUP_SPEC(Args),
        {component_sup,
         {component_supervisor, start_link, [Args]},
         temporary,
         10000,
         supervisor,
         [component_supervisor]}).

%% Component specification
-define(COMP_SPEC(Module, Args),
        {Module,
         {Module, start_link, [Args]},
         temporary,
         3000,
         worker,
         [Module]}).

-define(TOW_TRUCK_TIME, 30000).
-define(VEHICLE_DECELERATION, 2000).
    
-include("component.hrl").

-record(state, {supervisor,
                event_manager,
                event_handlers = [],
                route = []}).

%%% -------------------------- Interface Functions ------------------------- %%%

startup() ->
    ?MODULE ! startup.

initialize(Route, Components) ->
  ?MODULE ! {update_route, Route},
  ?MODULE ! {initialize_vehicle, Components}.

set_testing_environment(TestingComponent) ->
  ?MODULE ! {start_env_testing_component, TestingComponent}.

cause_mechanical_failure() ->
  gen_server:cast(?MODULE, breakdown).

start(SupPid) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [SupPid], []).

start_link(SupPid) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [SupPid], []).

start(Sup, Route, Components) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [Sup, Route, Components], []).

start_link(Sup, Route, Components) ->
  gen_server:start_link(
    {local, ?MODULE}, 
    ?MODULE, 
    [Sup, Route, Components], 
    []
  ).


%%% -------------------------- Callback Functions -------------------------- %%%

init([Sup]) ->
  {ok, #state{supervisor = Sup}};

init([Sup, Route, Components]) ->
  self() ! {update_route, Route},
  self() ! {initialize_vehicle, Sup, Components},
  {ok, #state{supervisor = Sup}}.

% Vehicle startup message received, start the vehicle!
handle_info(startup, State) ->
  internal:log(
    State#state.event_manager, 
    lists:concat(
      [ "Vehicle startup! Starting from "
      , erlang:hd(State#state.route)
      , " and directed to "
      , lists:last(State#state.route)
      ]
    )
  ),
  CurrentPosition = current_position(State#state.route),
  startup_vehicle(CurrentPosition, State),
  {noreply, State};

%% Start all necessary components and event handlers.
%% Necessary, see page 292 Learn You Some Erlang for Great Good!
handle_info({initialize_vehicle, Components}, State) ->
  Sup = State#state.supervisor,
  {ok, EvMan} = supervisor:start_child(Sup, ?EVENT_MAN_SPEC([])),
  {ok, SupPid} = supervisor:start_child(Sup, ?COMP_SUP_SPEC([])),
  EventHandlers = State#state.event_handlers ++ 
                  register_event_handler(EvMan, ?EVENT_HANDLER_IDS),
  start_components(SupPid, Components, EvMan),
  {noreply, State#state{event_manager = EvMan,
                           event_handlers = EventHandlers}};

%% Update the vehicle's route.
handle_info({update_route, Route}, State) ->
  {noreply, State#state{route = Route}};

%% Initialize the required component for updating the testing environment.
handle_info({start_env_testing_component, Component}, State) ->
  Sup = State#state.supervisor,
  EvMan = State#state.event_manager,
  start_component(Sup, Component, EvMan),
  {noreply, State};

handle_info(Msg, State) ->
  internal:a_log(State#state.event_manager,
               warn,
               ?MODULE,
               lists:concat(["Unknown msg: ", Msg])),
  {noreply, State}.

%% Position type received, now deal with different cases.
handle_call({position_type, Type}, _From, State) ->
  Pid = State#state.event_manager,
  case Type of
    normal -> 
      advance(Pid, State#state.route);
    intersection_entrance -> 
      resolve(Pid, Type);
    intersection_internal ->
      advance(Pid, State#state.route); 
    intersection_exit ->
      advance(Pid, State#state.route);
    start ->
      advance(Pid, State#state.route);
    finish -> 
      update_position(Pid, current_position(State#state.route), []), 
      internal:log(Pid, "Arrived at destination.");
    _ -> 
      internal:a_log(State#state.event_manager,
                     warn,
                     ?MODULE,
                     lists:concat(["Unknown position type: ", Type]))
  end,
  {reply, ack, State}.

%% Moved message received, clear to move forward in the path.
handle_cast(moved, State) ->
  EvMan = State#state.event_manager,
  OldRoute = State#state.route,
  NewRoute = make_a_step(OldRoute),
  internal:log( 
    EvMan, 
    lists:concat( ["Vehicle moved in position: ", hd( NewRoute )] )
  ),
  update_position(EvMan, hd(OldRoute), hd(NewRoute)),
  check_positon_type(EvMan, current_position(NewRoute)),
  {noreply, State#state{route = NewRoute}};

%% Cause vehicle breakdown (mechanical failure).
handle_cast(breakdown, State) ->
  % Sup = State#state.supervisor,
  EvMan = State#state.event_manager,
  internal:log(
    State#state.event_manager,
    "Mechanical failure. Tow truck will take care of me."
  ),
  internal:event(EvMan, notification, vehicle_breakdown, []),
  timer:sleep(?TOW_TRUCK_TIME),
  update_position(EvMan, current_position(State#state.route), []),
  {stop, normal, State};

handle_cast(_, State) -> {noreply, State}.

terminate(_Reason, State) ->
  EvMan = State#state.event_manager,
  HandlerIds = State#state.event_handlers,
  remove_event_handler(EvMan, HandlerIds),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% -------------------------- Private Functions --------------------------- %%%

%% Start components.
start_components(Sup, Components, EvMan) ->
  Fun = fun(C) -> start_component(Sup, C, EvMan) end,
  lists:map(Fun, Components).

%% Function to start a component C, returns the started component.
start_component(Sup, C = #component{module = Module}, EvMan) ->
  supervisor:start_child(Sup, ?COMP_SPEC(Module, C#component{
                                                      event_manager = EvMan,
                                                      supervisor = Sup})).

%% Function necessary to 'startup the vehicle', to start the whole vehicle
%% logic. Startup the vehicle from position 'Pos'.
startup_vehicle(Pos, State) ->
  case Pos of
    [] -> false;
    _ ->
      EvMan = State#state.event_manager,
      check_positon_type(EvMan, Pos),
      update_position(State#state.event_manager, [], Pos), 
      true
  end.

%% Adds a list of event handlers to the event manager.
register_event_handler(Pid, Handlers) ->
  Fun = fun(HandlerId) -> gen_event:add_handler(Pid, HandlerId, [self()]) end,
  lists:map(Fun, Handlers).

%% Remove all event handlers from the AV server.
remove_event_handler(Pid, Handlers) ->
  Fun = fun(HandlerId) -> gen_event:delete_handler(Pid, HandlerId, []) end,
  lists:map(Fun, Handlers),
  [].

%% The vehicle is ready to advance to the next position in the route.
advance(Pid, Route) ->
  Next = next_position(Route),
  case Next of
    {ok, Position} -> 
      timer:sleep( ?VEHICLE_DECELERATION ),
      internal:event(Pid, request, move, Position);
    _ -> internal:log(Pid, "Vehicle has arrived at destination. ~n")
  end.

check_positon_type(Pid, Position) ->
  internal:event(Pid, request, position_type, Position).

%% Resolve a node situation (free, stop), only stop needs to be resolved.
resolve(Pid, Type) ->
  internal:event(Pid, request, handle_position_type, Type).

%% Make a step in the route.
make_a_step(Route) ->
  tl(Route).

%% Get next position, if available, otherwise return a warning because the route
%% is completed.
next_position([_, Next|_]) -> {ok, Next};
next_position([]) -> {warning, route_empty};
next_position([_]) -> {warning, route_completed}.

%% Return the current position in the route.
current_position([]) -> [];
current_position(Route) -> hd(Route).

update_position(Pid, OldPos, NewPos) ->
  internal:event(Pid, notification, position_changed, {OldPos, NewPos}).

