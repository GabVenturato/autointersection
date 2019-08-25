%%% This module is the event handler for the AV's components listening to events
%%% sent by the AV's server.

-module(av_server_event_handler).
-behavior(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {components}). 

%TEMP, should be replaced with a common .hrl (header) file
-record(component, {name,
                    module,
                    pid}).

%%% -------------------------- Callback Functions -------------------------- %%%

init([Components]) -> 
  State = save_components(Components, #state{}),
  {ok, State}.

handle_event({startup, Position}, State) ->
  Pid = get_component_pid(av_recognition_component, State#state.components),
  gen_server:call(Pid, {check_position_status, Position}),
  {ok, State};

handle_event({route_completed}, State) ->
  % Do something.
  {ok, State};

handle_event({handle_status, Status}, State) ->
  Pid = get_component_pid(av_communication_component, State#state.components),
  gen_server:cast(Pid, {handle_status, Status}),
  {ok, State};

handle_event({move, Position}, State) ->
  Pid = get_component_pid(av_motion_component, State#state.components),
  gen_server:cast(Pid, {move, Position}),
  {ok, State}.

handle_call(_, State) -> {ok, ok, State}.
handle_info(_, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, _State) -> ok.

%%% -------------------------- Private Functions --------------------------- %%%

%% Get an AV component from state.
get_component(Key, Storage) ->
  case orddict:find(Key, Storage) of
     {ok, _} -> 
        orddict:fetch(Key, Storage);
     _ -> 
       terminate("No such component", [])
  end.

%% Get an AV component's pid.
get_component_pid(Key, Storage) ->
  Comp = get_component(Key, Storage),
  Pid = Comp#component.pid,
  Pid.

%% Save components in state.
save_components(Components, State) ->
  Orddict = orddict:new(),
  FoldingFun = fun(C = #component{module = Module}, Dict) -> 
                                orddict:append(Module, C, Dict) end,
  State = #state{components = lists:foldl(FoldingFun,
                                              Orddict,
                                              Components)}.