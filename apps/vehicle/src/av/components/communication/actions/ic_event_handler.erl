%%% This module is the event handler for the communication component listening to
%%% events sent by the AV server.

-module(ic_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, 
  terminate/2]).

-include("event.hrl").

%%% -------------------------- Callback Functions -------------------------- %%%

init([Pid]) -> 
  {ok, Pid}.

%% If vehicle finished the crossing
handle_event(#event
  { type = notification
  , name = position_type
  , content = intersection_exit}
  , Pid) ->
  gen_statem:cast( Pid, crossing_complete ),
  {ok, Pid};

%% If an internal mechanical failure is detected
handle_event(#event{type = notification, name = vehicle_breakdown}, Pid) ->
  gen_statem:cast( Pid, mechanical_failure ),
  {ok, Pid};

handle_event(_, State) -> {ok, State}.


handle_call(_, State) -> {reply, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.