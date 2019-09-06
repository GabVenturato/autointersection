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

-include("../../../../include/event.hrl").


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
  case Reason of
    normal -> io:format("Vehicle in front is down for normal reasons! ~n");
    _ ->
      io:format("Vehicle in front has sw crashed! Waiting for the tow truck... ~n"),
      EvManPid = State#component.event_manager,
      {_, Vehicle} = Pid,
      notify(EvManPid, #event{type = notification, name = vehicle_down, content = Vehicle})
  end,
  {noreply, State};


handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
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
      io:format("Waiting for next position: ~p ...~n", [Pos]),
      who_is_at(Pid, Pos)
  end.

%% Check what is the object in Position.
verify_position(Position, Object, State) ->
  case Object of
    {vehicle, VehicleInFront} -> 
      io:format("A vehicle is in front: ~p ...~n", [VehicleInFront]),
      MonitorRef = erlang:monitor(process, {vehicle_supervisor, VehicleInFront}),
      receive_position_update(Position, MonitorRef, State);
    _ ->  
      io:format("Overtaking the object in front...~n"),
      move(State#component.event_manager)
  end.

%% If we have to wait, receive updates from the sensor.
receive_position_update(Position, Ref, State) ->
  EvManPid = State#component.event_manager,
  ProbePid = State#component.probe,
  gen_server:cast(ProbePid, {notify_when_free, {EvManPid, Position, Ref}}).

%% Position update received, now handle the cases.
position_update(Update, MonitorRef, State) ->
  EvManPid = State#component.event_manager,
  case Update of 
    clear_to_move ->
      erlang:demonitor(MonitorRef),
      move(EvManPid);
    _ ->
      io:format("Vehicle stuck...~n")
  end.

%% Move to position.
move(Pid) ->
  notify(Pid, #event{type = notification, name = moved}).

who_is_at(Pid, Pos) ->
  notify(Pid, #event{type = request, name = who_is_at, content = Pos}).

%% Check with the sensor if the position is free.
is_position_free(ProbePid, Position) ->
  gen_server:call(ProbePid, {is_position_free, Position}).

register_event_handler(Pid, HandlerId) ->
  gen_event:add_handler(Pid, HandlerId, [self()]).

remove_event_handler(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId, []).

%% Generic async event notification.
notify(Pid, Msg) ->
  gen_event:notify(Pid, Msg).