%%% This module is the event handler for a process listening to motion
%%% events sent by the AV's motion component.

-module(av_motion_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

%%% -------------------------- Callback Functions -------------------------- %%%

init([Pid]) -> {ok, Pid}.

handle_event({moved}, Pid) ->
  gen_server:call(Pid, {moved}),
  {ok, Pid}.

handle_call(_, State) -> {ok, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.