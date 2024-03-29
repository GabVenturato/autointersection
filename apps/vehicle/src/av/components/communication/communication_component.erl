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
-include( "component.hrl" ).

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
  register_event_handler(
    CompDetails#component.event_manager,
    ?EVENT_HANDLER_ID
  ),
  {ok, CompDetails#component{handler = ?EVENT_HANDLER_ID}}.

handle_call(_, _From, State) -> {reply, ok, State}.

% Position type received, now handle the cases.
handle_cast({handle_position_type, Type}, State) ->
  case Type of
    intersection_entrance ->
      start_intersection_coordination(State);
    _ -> 
      internal:a_log(State#component.event_manager,
                     warn,
                     ?MODULE,
                     lists:concat(["Cannot handle position type: ", Type]))
  end,
  {noreply, State};

% Check who is at the specified position.
handle_cast({who_is_at, Position}, State) ->
  Pid = State#component.probe,
  Result = get_vehicle_at(Pid, Position),
  internal:event(State#component.event_manager, 
                 notification, vehicle_at, {Position, Result}),
  {noreply, State};

% Notify the environment about a vehicle down (sw failure).
handle_cast({vehicle_down, Vehicle}, State) ->
  Pid = State#component.probe,
  signal_vehicle_down(Pid, Vehicle),
  {noreply, State};

handle_cast(_, State) ->
  {noreply, State}.

handle_info(Msg, State) ->
  internal:a_log(State#component.event_manager,
                 warn,
                 ?MODULE,
                 lists:concat(["Unknown msg: ", Msg])),
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
  Probe = State#component.probe,
  EvMan = State#component.event_manager,
  internal:log(EvMan, "Solving intersection..."),
  {ok, SupPid} = supervisor:start_child(
    State#component.supervisor,
    ?SUP_SPEC([])
  ),
  supervisor:start_child(SupPid, ?INTER_CROSS_SPEC([Probe, EvMan])).

%% Returns the pid of the vehicle at the specified position.
get_vehicle_at(Pid, Position) ->
  gen_server:call(Pid, {vehicle_at, Position}).

%% Signals to the environment that the vehicle is down.
signal_vehicle_down(Pid, Vehicle) ->
  gen_server:cast(Pid, {vehicle_down, Vehicle}).

register_event_handler(Pid, HandlerId) ->
  gen_event:add_handler(Pid, HandlerId, [self()]).

remove_event_handler(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId,[]).
