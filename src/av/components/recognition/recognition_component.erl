%%% This module represents a generic AV component that deals with recognition 
%%% of objects in the environment. 
%%% All the logic related to object recognition should be treated by this 
%%% module:
%%% - the module can start new processes in order to handle more complex tasks
%%% - the component must report important events to the event manager only!
%%% Communication between various AV's components is handled only through
%%% the event manager by means of event notifications.

-module(recognition_component).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/1, start_link/1]).

% This component's event handler.
-define(EVENT_HANDLER_ID, {rec_event_handler, make_ref()}).

% Include component record.
-include("../../../../include/component.hrl").

%%% -------------------------- Interface Functions ------------------------- %%%

start(CompDetails) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [CompDetails], []).

start_link(CompDetails) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [CompDetails], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([CompDetails]) ->
  HandlerId = register_event_handler(CompDetails#component.event_manager, ?EVENT_HANDLER_ID),
  {ok, CompDetails#component{handler = HandlerId}}.

handle_call({check_position_status, Position}, _From, State) ->
  Pid = State#component.event_manager,
  Status = position_status(State#component.sensor, Position),
  notify(Pid, {coo, {position_status, Status}}),
  {reply, ok, State}.

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

%% Ask the sensor to return the position status of a given position.
position_status(SensorPid, Pos) ->
  gen_server:call(SensorPid, {position_status, Pos}).

register_event_handler(Pid, HandlerId) ->
  gen_event:add_handler(Pid, HandlerId, [self()]).

%% Generic synchronous event notification.
notify(Pid, Msg) ->
  gen_event:notify(Pid, Msg).