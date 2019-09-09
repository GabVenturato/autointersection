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
-include("component.hrl").

-include("event.hrl").


%%% -------------------------- Interface Functions ------------------------- %%%

start(CompDetails) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [CompDetails], []).

start_link(CompDetails) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [CompDetails], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([CompDetails]) ->
  register_event_handler(CompDetails#component.event_manager, ?EVENT_HANDLER_ID),
  {ok, CompDetails#component{handler = ?EVENT_HANDLER_ID}}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast({move, Position}, State) ->
  begin_moving(Position, State),
  {noreply, State};

handle_cast({vehicle_at, {Position, Result}}, State) ->
  verify_position(Position, Result, State),
  {noreply, State};

handle_cast({position_update, {Update, Ref}}, State) ->
  position_update(Update, Ref, State),
  {noreply, State};

handle_cast(_, State) -> {noreply, State}.

%%  The monitored vehicle went down, notify others about it.
handle_info({'DOWN', MonitorReference, process, Pid, Reason}, State) ->
  erlang:demonitor(MonitorReference),
  EvMan = State#component.event_manager,
  case Reason of
    normal ->
      internal:log(EvMan, "Vehicle in front is down for normal reasons!");
    _ ->
      internal:log(EvMan, "Vehicle in front has sw crashed! Waiting for the tow truck..."),
      {_, Vehicle} = Pid,
      internal:event(EvMan, notification, vehicle_down, Vehicle)
  end,
  {noreply, State};


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

%% Main function to move the vehicle.
begin_moving(Pos, State) ->
  Pid = State#component.event_manager,
  case is_position_free(State#component.probe, Pos) of 
    true ->
      move(Pid);
    false ->
      internal:log(Pid, lists:concat(["Waiting for next position: ", Pos])),
      who_is_at(Pid, Pos)
  end.

%% Check what is the object in Position.
verify_position(Position, Object, State) ->
  EvMan = State#component.event_manager,
  case Object of
    {vehicle, VehicleInFront} -> 
      internal:log(EvMan, lists:concat(["A vehicle is in front: ", VehicleInFront])),
      MonitorRef = erlang:monitor(process, {vehicle_supervisor, VehicleInFront}),
      receive_position_update(Position, MonitorRef, State);
    _ ->  
      internal:log(EvMan, "Overtaking the object in front..."),
      move(EvMan)
  end.

%% If we have to wait, receive updates from the sensor.
receive_position_update(Position, Ref, State) ->
  EvMan = State#component.event_manager,
  ProbePid = State#component.probe,
  gen_server:cast(ProbePid, {notify_when_free, {EvMan, Position, Ref}}).

%% Position update received, now handle the cases.
position_update(Update, MonitorRef, State) ->
  EvMan = State#component.event_manager,
  case Update of 
    clear_to_move ->
      erlang:demonitor(MonitorRef),
      move(EvMan);
    _ ->
      internal:a_log(State#component.event_manager,
                     warn,
                     ?MODULE,
                     lists:concat(["Vehicle stuck because of: ", Update]))
  end.

%% Move to position.
move(Pid) ->
  internal:event(Pid, notification, moved, []).

who_is_at(Pid, Pos) ->
  internal:event(Pid, request, who_is_at, Pos).

%% Check with the sensor if the position is free.
is_position_free(ProbePid, Position) ->
  gen_server:call(ProbePid, {is_position_free, Position}).

register_event_handler(Pid, HandlerId) ->
  gen_event:add_handler(Pid, HandlerId, [self()]).

remove_event_handler(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId, []).