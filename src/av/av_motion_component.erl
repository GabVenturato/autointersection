%%% This module represents a generic AV component that deals with movement. 
%%% All the logic related to movement should be treated by this module:
%%% - the module can start new processes in order to handle more complex tasks
%%% - the component must report important events to the event manager only!
%%% Communication between various AV's components is handled only through
%%% the event manager by means of event notifications.

-module(av_motion_component).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/1, start_link/1]).

-record(state, {sensor, event_maneger}).

% Sleep time used for busy waiting.
-define(SLEEP_TIME, 700).

%%% -------------------------- Interface Functions ------------------------- %%%

start([Sensors, EventManagerPid]) ->
  gen_server:start({local, av_communication_component}, ?MODULE, [Sensors, EventManagerPid], []).

start_link([Sensors, EventManagerPid]) ->
  gen_server:start_link({local, av_communication_component}, ?MODULE, [Sensors, EventManagerPid], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([Sensors, EventManagerPid]) ->
  {ok, #state{sensor = Sensors, event_maneger = EventManagerPid}}.

handle_call(_, _From, State) ->
  {reply, ok, State}.

handle_cast({move, Position}, State) ->
  move_to(Position, State),
  {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
  {ok, _State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% -------------------------- Private Functions --------------------------- %%%

%% Check with the sensor if the position is free.
is_position_free(SensorPid, Pos) ->
  gen_server:call(SensorPid, {is_position_free, Pos}).

%% Move to position Pos, notify an event when successfull. Retry until succeeds.
move_to(Pos, State) ->
  Pid = State#state.event_maneger,
  case is_position_free(State#state.sensor, Pos) of 
    true ->
      sync_notify(Pid, {moved});
    false ->
      timer:sleep(?SLEEP_TIME),
      move_to(Pos, State)   
  end.

%% Generic synchronous event notification.
sync_notify(Pid, Msg) ->
  gen_event:sync_notify(Pid, Msg).