%% This event handler is used by the env_testing_component in order to
%% notify the testing environment about vehicle state transitions.

-module(env_testing_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
 terminate/2]).

-define(HOSTNAME, element(2,inet:gethostname())).

-include("event.hrl").

%%% -------------------------- Callback Functions -------------------------- %%%

init([Pid]) -> {ok, Pid}.

handle_event(
    #event
      { type = notification
      , name = position_changed
      , content = {OldPos, NewPos}
      }, 
    Pid
  ) ->
  gen_server:call(Pid, {update_position, {OldPos, NewPos}}),
  {ok, Pid};

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.
