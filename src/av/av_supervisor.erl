%%% Supervisor module in charge of supervising the main AV's components:
%%% AV server, AV component supervisor and AV event manager.

-module(av_supervisor).

-behavior(supervisor).

-export([start_link/1]).

-export([init/1]).

%% Supervisor configuration details
-define(RESTART_STRATEGY, one_for_one).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

%%% -------------------------- Interface Functions ------------------------- %%%

start_link(Config) ->
  supervisor:start_link({local,?MODULE}, ?MODULE, Config).

%%% -------------------------- Callback Functions -------------------------- %%%

init({Route, Components, Sensors}) ->
  {ok, {{?RESTART_STRATEGY, ?MAX_RESTART, ?MAX_TIME},
        [{av_serv,
          {av_server, start_link, [self(), Route, Components, Sensors]},
          permanent, 1000, worker, [av_server]}
        ]}}.