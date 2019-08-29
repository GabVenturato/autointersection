%%% This module is the event handler for the motion component listening to
%%% events sent by the AV server.

-module(mot_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).


%%% -------------------------- Callback Functions -------------------------- %%%

init([Pid]) -> 
  {ok, Pid}.

handle_event({mot, Msg = {move, _}}, Pid) ->
  gen_server:cast(Pid, Msg),
  {ok, Pid};

handle_event({mot, Msg = {object_at, {_, _}}}, Pid) ->
  gen_server:cast(Pid, Msg),
  {ok, Pid};
  

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.