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

-include("../../../../include/event.hrl").

-define(INTERSECTION_SOLVING_MODULE, intersection_coordination).

%% Action supervisor specification.
-define(SUP_SPEC(Args),
        {action_sup,
         {action_supervisor, start_link, [Args]},
         transient,
         10000,
         supervisor,
         [action_supervisor]}).

%% Intersection crossing process specification.
-define(INTER_CROSS_SPEC(Args),
        {?INTERSECTION_SOLVING_MODULE,
         {?INTERSECTION_SOLVING_MODULE, start_link, Args},
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
  register_event_handler(CompDetails#component.event_manager, ?EVENT_HANDLER_ID),
  {ok, CompDetails#component{handler = ?EVENT_HANDLER_ID}}.

handle_call(_, _From, State) -> {reply, ok, State}.

handle_cast({handle_position_type, Type}, State) ->
  case Type of
    intersection_entrance ->
      start_intersection_coordination(State);
    _ -> io:format("Unknown type: ~p~n", [Type])
  end,
  {noreply, State};

handle_cast({who_is_at, Position}, State) ->
  Pid = State#component.probe,
  Result = get_vehicle_at(Pid, Position),
  notify(State#component.event_manager, 
         #event{type = notification, name = vehicle_at, content = {Position, Result}}),
  {noreply, State};

handle_cast({vehicle_down, VehiclePid}, State) ->
  Pid = State#component.probe,
  signal_vehicle_down(Pid, VehiclePid),
  {noreply, State};

handle_cast(_, State) ->
  {noreply, State}.

handle_info(Msg, State) ->
  io:format("Unknown msg: ~p~n", [Msg]),
  {noreply, State}.

terminate(_Reason, State) ->
  remove_event_handler(State#component.event_manager, State#component.handler),
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% -------------------------- Private Functions --------------------------- %%%

%% Start the process that is in charge of solving the intersection.
start_intersection_coordination(State) ->
  %% Spawn new process to begin coordination.
  io:format("Solving intersection... ~n"),
  SensorPid = State#component.probe,
  EvManPid = State#component.event_manager,
  {ok, SupPid} = supervisor:start_child(State#component.supervisor, ?SUP_SPEC([])),
  supervisor:start_child(SupPid, ?INTER_CROSS_SPEC([SensorPid, EvManPid])).

  %% Temporary for testing purposes:
  % io:format("Solving intersection... ~n"),
  % Number = rand:uniform(1800),
  % timer:sleep(7000-Number),
  % EvManPid = State#component.event_manager,
  % notify(EvManPid, 
  %        #event{type = notification, name = position_type, content = normal}).

get_vehicle_at(Pid, Position) ->
  gen_server:call(Pid, {vehicle_at, Position}).

signal_vehicle_down(Pid, VehiclePid) ->
  gen_server:cast(Pid, {vehicle_down, VehiclePid}).

register_event_handler(Pid, HandlerId) ->
  gen_event:add_handler(Pid, HandlerId, [self()]).

remove_event_handler(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId,[]).

%% Generic synchronous event notification.
notify(Pid, Msg) ->
  gen_event:notify(Pid, Msg).
