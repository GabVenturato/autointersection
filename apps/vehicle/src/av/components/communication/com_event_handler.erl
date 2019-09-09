%%% This module is the event handler for the communication component listening to
%%% events sent by the AV server.

-module(com_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include("event.hrl").

%%% -------------------------- Callback Functions -------------------------- %%%

init([Pid]) -> 
  {ok, Pid}.

handle_event(#event{type = request, name = handle_position_type, content = Type}, Pid) ->
  gen_server:cast(Pid, {handle_position_type, Type}),
  {ok, Pid};

handle_event(#event{type = notification, name = vehicle_down, content = Vehicle}, Pid) ->
  gen_server:cast(Pid, {vehicle_down, Vehicle}),
  {ok, Pid};

handle_event(#event{type = request, name = who_is_at, content = Pos}, Pid) -> 
  gen_server:cast(Pid, {who_is_at, Pos}),
  {ok, Pid};

handle_event(_, State) -> {ok, State}.


handle_call(_, State) -> {reply, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.