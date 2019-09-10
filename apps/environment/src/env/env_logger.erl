%%% This module represents a generic AV component that logs internal vehicle
%%% events. 
%%% All internal logging should be handled by this module:
%%% - the module can start new processes in order to handle more complex tasks
%%% - the component must report important events to the event manager only!
%%% Communication between various AV's components is handled only through
%%% the event manager by means of event notifications.

-module(env_logger).
-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/0, start_link/0]).


-define( LOG_FOLDER, "log/").


%%% -------------------------- Interface Functions ------------------------- %%%

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% -------------------------- Callback Functions -------------------------- %%%

init([]) ->
  % Create log directory and open log file
  file:make_dir( ?LOG_FOLDER ),
  {ok, FP} = file:open( 
    ?LOG_FOLDER ++ atom_to_list( node() ) ++ ".log", 
    [write] 
  ),
  {ok, FP}.

%% Logs of level "debug" are saved only in the log file
handle_call({log, Tag, Msg}, _From, FP) ->
  {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
  Date = lists:concat(
    [Year, "-", Month, "-", Day, "::", Hour, ":", Min, ":", Sec]
  ),
  io:format( 
    FP,
    "[~p - ~p] ENVIRONMENT: ~p: --- ~p --- ~n",
    [Date, node(), Tag, Msg]
  ),
  {reply, ok, FP}.

handle_cast(_, FP) ->
  {noreply, FP}.

handle_info(Msg, FP) ->
  io:format(FP, "ENVIRONMENT - Unknown msg: ", [Msg]),
  {noreply, FP}.

terminate(_Reason, Fd) ->
  file:close( Fd ),
  {ok, []}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.