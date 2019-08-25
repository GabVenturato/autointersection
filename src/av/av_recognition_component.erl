%%% This module represents a generic AV component that deals with recognition 
%%% of objects in the environment. 
%%% All the logic related to object recognition should be treated by this 
%%% module:
%%% - the module can start new processes in order to handle more complex tasks
%%% - the component must report important events to the event manager only!
%%% Communication between various AV's components is handled only through
%%% the event manager by means of event notifications.

-module(av_recognition_component).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/1, start_link/1]).

-record(state, {sensor, event_maneger}).

%%% -------------------------- Interface Functions ------------------------- %%%

start([Sensors, EventManagerPid]) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [Sensors, EventManagerPid], []).

start_link([Sensors, EventManagerPid]) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Sensors, EventManagerPid], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([Sensors, EventManagerPid]) ->
  {ok, #state{sensor = Sensors, event_maneger = EventManagerPid}}.

handle_call({check_position_status, Position}, _From, State) ->
  Pid = State#state.event_maneger,
  Status = position_status(Position, State),
  sync_notify(Pid, {position_status, Status}),
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
position_status(Pos, State) ->
  gen_server:call(State#state.sensor, {position_status, Pos}).

%% Generic synchronous event notification.
sync_notify(Pid, Msg) ->
  gen_event:sync_notify(Pid, Msg).