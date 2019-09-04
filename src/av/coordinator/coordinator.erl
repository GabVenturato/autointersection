-module(coordinator).
-behavior(gen_server).

% Interface functions.
-export([startup/0, initialize/2, set_testing_environment/1]).
-export([start/1, start_link/1, start/3, start_link/3]).

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
         permanent,
         10000,
         worker,
         [gen_event]}).

%% Component supervisor specification
-define(COMP_SUP_SPEC(Args),
        {component_sup,
         {component_supervisor, start_link, [Args]},
         permanent,
         10000,
         supervisor,
         [component_supervisor]}).

%% Component specification
-define(COMP_SPEC(Module, Args),
        {Module,
         {Module, start_link, [Args]},
         permanent,
         3000,
         worker,
         [Module]}).

-define(TOW_TRUCK_TIME, 40000).
    
-include("../../../include/component.hrl").
-include("../../../include/event.hrl").

-record(internal, {supervisor,
                   event_manager,
                   event_handlers = [],
                   route = []}).

%%% -------------------------- Interface Functions ------------------------- %%%

startup() ->
    ?MODULE ! startup.

initialize(Route, Components) ->
  ?MODULE ! {update_route, Route},
  ?MODULE ! {initialize_vehicle, Components}.

set_testing_environment(EnvLocation) ->
  ?MODULE ! {start_environment_listener, EnvLocation}.

start(SupPid) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [SupPid], []).

start_link(SupPid) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [SupPid], []).

start(Sup, Route, Components) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [Sup, Route, Components], []).

start_link(Sup, Route, Components) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Sup, Route, Components], []).


%%% -------------------------- Callback Functions -------------------------- %%%

init([Sup]) ->
  {ok, #internal{supervisor = Sup}};

init([Sup, Route, Components]) ->
  self() ! {update_route, Route},
  self() ! {initialize_vehicle, Sup, Components},
  {ok, #internal{supervisor = Sup}}.

handle_info(startup, State) ->
  io:format("Startup msg received. ~n"),
  CurrentPosition = current_position(State#internal.route),
  startup_vehicle(CurrentPosition, State),
  {noreply, State};

%% Start all necessary components and event handlers.
%% Necessary, see page 292 Learn You Some Erlang for Great Good!
handle_info({initialize_vehicle, Components}, State) ->
  Sup = State#internal.supervisor,
  {ok, EvMan} = supervisor:start_child(Sup, ?EVENT_MAN_SPEC([])),
  {ok, SupPid} = supervisor:start_child(Sup, ?COMP_SUP_SPEC([])),
  EventHandlers = State#internal.event_handlers ++ 
                  register_event_handler(EvMan, ?EVENT_HANDLER_IDS),
  start_components(SupPid, Components, EvMan),
  {noreply, State#internal{event_manager = EvMan,
                           event_handlers = EventHandlers}};

handle_info({update_route, Route}, State) ->
  {noreply, State#internal{route = Route}};

handle_info({start_environment_listener, NodeName}, State) ->
  EvManPid = State#internal.event_manager,
  gen_event:add_handler(EvManPid, ?ENV_HANDLER, [NodeName]),
  {noreply, State};

handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

%% Deal with position type.
handle_call({position_type, Type}, _From, State) ->
  Pid = State#internal.event_manager,
  case Type of
    normal -> 
      advance(Pid, State#internal.route);
    intersection_entrance -> 
      resolve(Pid, Type);
    intersection_exit ->
      advance(Pid, State#internal.route);
    start ->
      advance(Pid, State#internal.route);
    finish -> 
      update_position(Pid, State#internal.supervisor, current_position(State#internal.route), []), 
      io:format("Arrived at destination. ~n");
    _ -> 
      io:format("Cannot handle such position type! ~p~n", [Type])
  end,
  {reply, ack, State}.

%% Moved message received, clear to move forward in the path.
handle_cast(moved, State) ->
  EvMan = State#internal.event_manager,
  Sup = State#internal.supervisor,
  OldRoute = State#internal.route,
  NewRoute = make_a_step(OldRoute),
  io:format("Vehicle moved in position: ~p~n", [hd(NewRoute)]),
  update_position(EvMan, Sup, hd(OldRoute), hd(NewRoute)),
  check_positon_type(EvMan, current_position(NewRoute)),
  {noreply, State#internal{route = NewRoute}};

handle_cast(breakdown, State) ->
  io:format("Mechanical failure"),
  Sup = State#internal.supervisor,
  EvMan = State#internal.event_manager,
  notify(EvMan, #event{type = notification, name = vehicle_breakdown}),
  supervisor:terminate_child(Sup, component_sup),
  timer:sleep(?TOW_TRUCK_TIME),
  update_position(EvMan, Sup, current_position(State#internal.route), []),
  terminate("Mechanical failure.", State),
  {noreply, State};

handle_cast(_, State) -> {noreply, State}.

terminate(_Reason, State) ->
  EvMan = State#internal.event_manager,
  HandlerIds = State#internal.event_handlers,
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

%% Function necessary to 'startup the vehicle', to start the whole vehicle logic.
%% Startup the vehicle from position 'Pos'.
startup_vehicle(Pos, State) ->
  case Pos of
    [] -> false;
    _ ->
      EvMan = State#internal.event_manager,
      check_positon_type(EvMan, Pos),
      update_position(State#internal.event_manager,
                      State#internal.supervisor,
                      [],
                      Pos), 
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
      notify(Pid, #event{type = request, name = move, content = Position});
    _ -> io:format("Vehicle has arrived at destination. ~n")
  end.

check_positon_type(Pid, Position) ->
  notify(Pid, #event{type = request, name = position_type, content = Position}).

%% Resolve a node situation (free, stop), only stop needs to be resolved.
resolve(Pid, Type) ->
  notify(Pid, #event{type = request, name = handle_position_type, content = Type}).

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

update_position(Pid, VehiclePid, OldPos, NewPos) ->
  notify(Pid, 
        #event{type = notification,
              name = position_changed,
              content = {VehiclePid, OldPos, NewPos}
        }).

%% Generic async event notification.
notify(Pid, Event) ->
  gen_event:notify(Pid, Event).

