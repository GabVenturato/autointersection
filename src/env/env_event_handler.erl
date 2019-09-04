%%% FIRST ARGUMENT OF gen_server:call() MUST BE CHANGED TO THE APPROPRIATE
%%% {environment, Node} TUPLE !!!!!

-module(env_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
 terminate/2]).

-define(HOSTNAME, element(2,inet:gethostname())).

-include("../../include/event.hrl").

%%% -------------------------- Callback Functions -------------------------- %%%

init([EnvLocation]) -> {ok, EnvLocation}.

handle_event(#event{type = notification, name = position_changed, content = {Pid, OldPos, NewPos}}, EnvLocation) ->
  gen_server:call(EnvLocation, {release_position, OldPos}),
  gen_server:call(EnvLocation, {occupy_position, Pid, NewPos}),
  {ok, EnvLocation};

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.