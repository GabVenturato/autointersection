%%% This module represents a generic AV component that deals with AV_to_AV 
%%% communication. 
%%% All the logic related to AV_to_AV communication should be treated by this 
%%% module:
%%% - the module can start new processes in order to handle more complex tasks
%%% - the component must report important events to the event manager only!
%%% Communication between various AV's components is handled only through
%%% the event manager by means of event notifications.

-module(communication_component).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/1, start_link/1]).

% This component's event handler.
-define(EVENT_HANDLER_ID, {com_event_handler, make_ref()}).

% Include component record.
-include("../../../../include/component.hrl").

-define(INTERSECTION_SOLVING_MODULE, module_name).

%% Process supervisor specification.
-define(SUP_SPEC(Args),
        {auxilary_sup,
         {auxilary_supervisor, start_link, [Args]},
         transient,
         10000,
         supervisor,
         [auxilary_supervisor]}).

%% Intersection crossing process specification.
-define(INTER_CROSS_SPEC(Args),
        {?INTERSECTION_SOLVING_MODULE,
         {?INTERSECTION_SOLVING_MODULE, start_link, [Args]},
         transient,
         5000,
         worker,
         [?INTERSECTION_SOLVING_MODULE]}).

%%% -------------------------- Interface Functions ------------------------- %%%

start(CompDetails) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [CompDetails], []).

start_link(CompDetails) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [CompDetails], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([CompDetails]) ->
  HandlerId = register_event_handler(CompDetails#component.event_manager, ?EVENT_HANDLER_ID),
  {ok, CompDetails#component{handler = HandlerId}}.

handle_call(_, _From, State) -> {reply, ok, State}.

handle_cast({handle_status, Status}, State) ->
  case Status of
    stop ->
      start_intersection_coordination(State);
    _ -> io:format("Unknown Status: ~p~n", [Status])
  end,
  {noreply, State};

handle_cast({who_is_at, Position}, State) ->
  Pid = State#component.sensor,
  Result = get_object_at(Pid, Position),
  notify(State#component.event_manager, {mot, {object_at, {Position, Result}}}),
  {noreply, State};

handle_cast({vehicle_down, VehiclePid}, State) ->
  Pid = State#component.sensor,
  signal_vehicle_down(Pid, VehiclePid),
  {noreply, State};

handle_cast(_, State) ->
  {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% -------------------------- Private Functions --------------------------- %%%

%% Start the process that is in charge of solving the intersection.
start_intersection_coordination(State) ->
  %% Spawn new process to begin coordination.
  EvManPid = State#component.event_manager,
  SensorPid = State#component.sensor,
  {ok, SupPid} = supervisor:start_child(State#component.supervisor, ?SUP_SPEC([])),
  supervisor:start_child(SupPid, ?INTER_CROSS_SPEC([SensorPid, EvManPid])).
  %% Temporary for testing purposes:
  %io:format("Solving intersection... ~n"),
  %gen_server:cast(State#component.sensor, {solve_intersection, EvManPid}).

get_object_at(Pid, Position) ->
  gen_server:call(Pid, {object_at, Position}).

signal_vehicle_down(Pid, VehiclePid) ->
  gen_server:cast(Pid, {vehicle_down, VehiclePid}).

register_event_handler(Pid, HandlerId) ->
  gen_event:add_handler(Pid, HandlerId, [self()]).

%% Generic synchronous event notification.
notify(Pid, Msg) ->
  gen_event:notify(Pid, Msg).