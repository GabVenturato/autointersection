%%% This module is the event handler for the recognition component listening to 
%%% relevant events.

-module(rec_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-include("event.hrl").

%%% -------------------------- Callback Functions -------------------------- %%%

init([Pid]) -> 
  {ok, Pid}.

handle_event(#event{type = request, name = position_type, content = Position}, Pid) ->
  gen_server:call(Pid, {position_type, Position}),
  {ok, Pid};

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.