%%% This module represents a generic AV component that deals with AV_to_AV 
%%% communication. 
%%% All the logic related to AV_to_AV communication should be treated by this 
%%% module:
%%% - the module can start new processes in order to handle more complex tasks
%%% - the component must report important events to the event manager only!
%%% Communication between various AV's components is handled only through
%%% the event manager by means of event notifications.

-module(av_communication_component).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/2, start_link/1]).

-record(state, {sensor, event_maneger}).

%%% -------------------------- Interface Functions ------------------------- %%%

start(Route, EnvPid) ->
  gen_server:start({local, ?MODULE}, ?MODULE, [Route, EnvPid], []).

start_link([Sensors, EventManagerPid]) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Sensors, EventManagerPid], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([Sensors, EventManagerPid]) ->
  {ok, #state{sensor = Sensors, event_maneger = EventManagerPid}}.

handle_call(_, _From, State) -> {ok, ok, State}.

handle_cast({handle_status, Status}, State) ->
  case Status of
    stop ->
      start_intersection_coordination(State);
    _ -> io:format("Unknown Status: ~p~n", [Status])
  end,
  {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% -------------------------- Private Functions --------------------------- %%%

%% Start the process that is in charge of solving the intersection.
start_intersection_coordination(State) ->
  %% Spawn new process to begin coordination.
  %% Temporary for testing purposes:
  Pid = State#state.event_maneger,
  sync_notify(Pid, {position_status, free}).

%% Generic synchronous event notification.
sync_notify(Pid, Msg) ->
  gen_event:sync_notify(Pid, Msg).