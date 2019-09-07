%%% This module represents a generic AV component that logs internal vehicle
%%% events. 
%%% All internal logging should be handled by this module:
%%% - the module can start new processes in order to handle more complex tasks
%%% - the component must report important events to the event manager only!
%%% Communication between various AV's components is handled only through
%%% the event manager by means of event notifications.

-module(logging_component).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/1, start_link/1]).

% This component's event handler.
-define(EVENT_HANDLER_ID, {log_event_handler, make_ref()}).

% Include component record.
-include("../../../../include/component.hrl").

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
  file:make_dir(log),
  {ok, FP} = file:open( "log/" ++ atom_to_list( node() ) ++ ".log", [write] ),
  {ok, CompDetails#component{handler = ?EVENT_HANDLER_ID, probe = FP}}.

%% Logs of type "debug" are saved only in the log file
handle_call({debug, Tag, Msg}, _From, State) ->
  io:format( 
    State#component.probe,
    "[~p] ~p: ~p: --- ~p --- ~n",
    [node(), string:uppercase( atom_to_list( internal ) ), Tag, Msg]
  ),
  {reply, ok, State};

handle_call({Level, Tag, Msg}, _From, State) ->
  io:format( 
    State#component.probe,
    "[~p] ~p: ~p: --- ~p --- ~n",
    [node(), string:uppercase( atom_to_list( Level ) ), Tag, Msg]
  ),
  io:format( 
    "[~p] ~p: ~p: --- ~p --- ~n",
    [node(), string:uppercase( atom_to_list( Level ) ), Tag, Msg]
  ),
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
  file:close( State#component.probe ),
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% -------------------------- Private Functions --------------------------- %%%

register_event_handler(Pid, HandlerId) ->
  gen_event:add_handler(Pid, HandlerId, [self()]).

remove_event_handler(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId, []).