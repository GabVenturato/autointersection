%%% This module represents a generic AV component that logs internal vehicle
%%% events. 
%%% All internal logging should be handled by this module:
%%% - the module can start new processes in order to handle more complex tasks
%%% - the component must report important events to the event manager only!
%%% Communication between various AV's components is handled only through
%%% the event manager by means of event notifications.

-module(env_testing_component).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/1, start_link/1]).

% This component's event handler.
-define(EVENT_HANDLER_ID, {env_testing_event_handler, make_ref()}).

% Include component record.
-include("component.hrl").

%%% -------------------------- Interface Functions ------------------------- %%%

start(CompDetails) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [CompDetails], []).

start_link(CompDetails) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [CompDetails], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([CompDetails]) ->
  register_event_handler(CompDetails#component.event_manager, ?EVENT_HANDLER_ID),
  {ok, CompDetails#component{handler = ?EVENT_HANDLER_ID}}.

handle_call({update_position, {OldPos, NewPos}}, _From, State) ->
  gen_server:call(State#component.probe, {update_position, {node(), OldPos, NewPos}}),
  {reply, ok, State}.

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

register_event_handler(Pid, HandlerId) ->
  gen_event:add_handler(Pid, HandlerId, [self()]).

remove_event_handler(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId, []).