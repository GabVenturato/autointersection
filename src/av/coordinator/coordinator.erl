-module(coordinator).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start/1, start_link/1, start/3, start_link/3]).

%$ Event Handlers (Listeners) registered by the coordinator.
-define(EVENT_HANDLER_IDS, [
  {communication_event_handler, make_ref()},
  {motion_event_handler, make_ref()},
  {recognition_event_handler, make_ref()}
]).

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
    
-include("../../../include/component.hrl").

-record(internal, {event_manager,
                   event_handlers,
                   route}).

%%% -------------------------- Interface Functions ------------------------- %%%

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
  self() ! {initialize_vehicle, Sup, []},
  {ok, #internal{route = []}};

init([Sup, Route, Components]) ->
  self() ! {initialize_vehicle, Sup, Components},
  {ok, #internal{route = Route}}.

%% Start all necessary components and event handlers.
%% Necessary, see page 292 Learn You Some Erlang for Great Good!
handle_info({initialize_vehicle, Sup, Components}, State) ->
  {ok, EvManPid} = supervisor:start_child(Sup, ?EVENT_MAN_SPEC([])),
  {ok, SupPid} = supervisor:start_child(Sup, ?COMP_SUP_SPEC([])),
  add_event_handlers(EvManPid),
  start_components(SupPid, Components, EvManPid),
  {noreply, State#internal{event_manager = EvManPid,
                           event_handlers = ?EVENT_HANDLER_IDS}};

handle_info({startup}, State) ->
  CurrentPosition = current_position(State#internal.route),
  EvManPid = State#internal.event_manager,
  startup(EvManPid, CurrentPosition),
  {noreply, State};

handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

%% Deal with position status (free, stop).
handle_call({position_status, Status}, _From, State) ->
  Pid = State#internal.event_manager,
  case Status of
    free -> 
      advance(Pid, State#internal.route);
    stop -> 
      resolve(Pid, stop);
    _ -> 
      io:format("Cannot handle such Status! ~p~n", [Status])
  end,
  {reply, ack, State}.

%% Moved message received, clear to move forward in the path.
handle_cast({moved}, State) ->
  NewRoute = make_a_step(State#internal.route),
  check_positon_status(State#internal.event_manager, current_position(NewRoute)),
  {noreply, State#internal{route = NewRoute}};

handle_cast(_, State) -> {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% -------------------------- Private Functions --------------------------- %%%

%% Start components.
start_components(Sup, Components, EvManPid) ->
  Fun = fun(C) -> start_component(Sup, C, EvManPid) end,
  lists:map(Fun, Components).

%% Function to start a component C, returns the started component.
start_component(Sup, C = #component{module = Module}, EvManPid) ->
  supervisor:start_child(Sup, ?COMP_SPEC(Module, C#component{
                                                      event_manager = EvManPid,
                                                      supervisor = Sup})).

%% Function necessary to 'startup the vehicle', to start the whole vehicle logic.
%% Startup the vehicle from position 'Pos'.
startup(Pid, Pos) ->
  check_positon_status(Pid, Pos).

%% Adds a list of event handlers to the event manager.
add_event_handlers(Pid) ->
  Fun = fun(HandlerId) -> gen_event:add_handler(Pid, HandlerId, [self()]) end,
  lists:map(Fun, ?EVENT_HANDLER_IDS).

%% Remove all event handlers from the AV server.
%remove_event_handlers(Pid, S) ->
%  HandlerIds = S#internal.event_handlers,
%  FoldingFun = fun(HandlerId, Acc) -> [gen_event:delete_handler(Pid, HandlerId)|Acc] end,
%  lists:foldl(FoldingFun, [], HandlerIds),
%  S#internal{event_handlers = []}.

%% The vehicle is ready to advance to the next position in the route.
advance(Pid, Route) ->
  Next = next_position(Route),
  case Next of
    {ok, Position} -> notify(Pid, {mot, {move, Position}}),
    io:format("VEHICLE MOVING!~n");
    _ -> notify(Pid, {gen, {route_completed}}),
         io:format("VEHICLE FINISHED!~n")
         %hd(Route) % Temp, returns current node to avoid errors.
  end.

check_positon_status(Pid, Position) ->
  notify(Pid, {rec, {check_position_status, Position}}).

%% Resolve a node situation (free, stop), only stop needs to be resolved.
resolve(Pid, stop) ->
  notify(Pid, {com, {handle_status, stop}}).

%% Make a step in the route.
make_a_step(Route) ->
  tl(Route).

%% Get next position, if available, otherwise return a warning because the route
%% is completed.
next_position([_, Next|_]) -> {ok, Next};
next_position([]) -> {warning, route_empty};
next_position([_]) -> {warning, route_completed}.


%% Return the current position in the route.
current_position(Route) -> hd(Route).

%% Generic synchronous event notification.
notify(Pid, Msg) ->
  gen_event:notify(Pid, Msg).

