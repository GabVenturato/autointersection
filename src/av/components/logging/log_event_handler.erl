%%% This module is the event handler for a component listening to internal log
%%% events.

-module(log_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include("../../../../include/event.hrl").

%%% -------------------------- Callback Functions -------------------------- %%%

init([Pid]) -> 
  {ok, Pid}.

handle_event(#event{type = {log, Level}, name = Tag, content = Msg}, Pid) ->
  gen_server:call(Pid, {Level, Tag, Msg}),
  {ok, Pid};

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.