%%% This module is the event handler for a process listening to recognition
%%% events sent by the AV's recognition component.

-module(recognition_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
 terminate/2]).

-include("event.hrl").

%%% -------------------------- Callback Functions -------------------------- %%%

init([Pid]) -> {ok, Pid}.

handle_event(#event{type = notification, name = position_type, content = Type}, Pid) ->
  gen_server:call(Pid, {position_type, Type}),
  {ok, Pid};

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.