%%% This module is the event handler for the motion component listening to
%%% relevant events.

-module(mot_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include("event.hrl").

%%% -------------------------- Callback Functions -------------------------- %%%

init([Pid]) -> 
  {ok, Pid}.

handle_event(#event{type = request, name = move, content = Position}, Pid) ->
  gen_server:cast(Pid, {move, Position}),
  {ok, Pid};

handle_event(#event{type = notification, name = vehicle_at, content = Vehicle}, Pid) ->
  gen_server:cast(Pid, {vehicle_at, Vehicle}),
  {ok, Pid};

handle_event(#event{type = notification, name = clear_to_move, content = Response}, Pid) ->
  gen_server:cast(Pid, {position_update, {clear_to_move, Response}}),
  {ok, Pid};

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.