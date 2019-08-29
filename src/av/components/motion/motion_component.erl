%%% This module represents a generic AV component that deals with movement. 
%%% All the logic related to movement should be treated by this module:
%%% - the module can start new processes in order to handle more complex tasks
%%% - the component must report important events to the event manager only!
%%% Communication between various AV's components is handled only through
%%% the event manager by means of event notifications.

-module(motion_component).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/1, start_link/1]).

% This component's event handler.
-define(EVENT_HANDLER_ID, {mot_event_handler, make_ref()}).

% Include component record.
-include("../../../../include/component.hrl").

% Sleep time used for busy waiting.
-define(TIME_TO_WAIT, 1200).

%%% -------------------------- Interface Functions ------------------------- %%%

start(CompDetails) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [CompDetails], []).

start_link(CompDetails) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [CompDetails], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([CompDetails]) ->
  HandlerId = register_event_handler(CompDetails#component.event_manager, ?EVENT_HANDLER_ID),
  {ok, CompDetails#component{handler = HandlerId}}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast({move, Position}, State) ->
  begin_moving(Position, State),
  {noreply, State};

handle_cast({object_at, {Position, Result}}, State) ->
  verify_position(Position, Result, State),
  {noreply, State};

handle_cast({position_update, {Update, Ref}}, State) ->
  position_update(Update, Ref, State),
  {noreply, State};

handle_cast(_, State) -> {noreply, State}.

%%  The monitored vehicle went down, notify others about it.
handle_info({'DOWN', MonitorReference, process, Pid, _Reason}, State) ->
  erlang:demonitor(MonitorReference),
  EvManPid = State#component.event_manager,
  notify(EvManPid, {com, {vehicle_down, Pid}}),
  io:format("Vehicle in front is down! ~n"),
  {noreply, State};
  
handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
  {ok, _State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% -------------------------- Private Functions --------------------------- %%%

%% Main function to move the vehicle.
begin_moving(Pos, State) ->
  Pid = State#component.event_manager,
  case is_position_free(State#component.sensor, Pos) of 
    true ->
      move(Pid);
    false ->
      io:format("Waiting for next position...~n"),
      notify(Pid, {com, {who_is_at, Pos}})
  end.

%% Check what is the object in Position.
verify_position(Position, Object, State) ->
  case Object of
    {vehicle, Pid} -> 
      io:format("A vehicle is in front...~n"),
      Ref = erlang:monitor(process, Pid),
      receive_position_update(Position, Ref, State);
    _ ->  
      io:format("Overtaking the object in front...~n"),
      move(State#component.event_manager)
  end.

%% If we have to wait, receive updates from the sensor.
receive_position_update(Position, Ref, State) ->
  EvManPid = State#component.event_manager,
  gen_server:cast(EvManPid, {notify_when_free, {self(), Position, Ref}}).

%% Position update received, now handle the cases.
position_update(Update, Ref, State) ->
  EvManPid = State#component.event_manager,
  case Update of 
    clear_to_move ->
      erlang:demonitor(Ref),
      move(EvManPid);
    _ ->
      io:format("Vehicle stuck...~n")
  end.

%% Move to position.
move(Pid) ->
  notify(Pid, {coo, {moved}}).

%% Check with the sensor if the position is free.
is_position_free(SensorPid, Position) ->
  gen_server:call(SensorPid, {is_position_free, Position}).

register_event_handler(Pid, HandlerId) ->
    gen_event:add_handler(Pid, HandlerId, [self()]).

%% Generic synchronous event notification.
notify(Pid, Msg) ->
  gen_event:notify(Pid, Msg).