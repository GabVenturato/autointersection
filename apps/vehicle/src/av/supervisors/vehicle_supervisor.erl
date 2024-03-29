%%% Supervisor module in charge of supervising the main AV's components:
%%% AV server, AV component supervisor and AV event manager.

-module(vehicle_supervisor).

-behavior(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

%% Supervisor configuration details
-define(RESTART_STRATEGY, one_for_one).
-define(MAX_RESTART, 0).
-define(MAX_TIME, 60).

%%% -------------------------- Interface Functions ------------------------- %%%

start_link() -> 
  supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_link(Config) ->
  supervisor:start_link({local,?MODULE}, ?MODULE, Config).

%%% -------------------------- Callback Functions -------------------------- %%%

init([]) ->
    {ok, {{?RESTART_STRATEGY, ?MAX_RESTART, ?MAX_TIME},
          [{coord,
            {coordinator, start_link, [self()]},
            temporary, 1000, worker, [coordinator]}
          ]}};

init({Route, Components}) ->
  {ok, {{?RESTART_STRATEGY, ?MAX_RESTART, ?MAX_TIME},
        [{coord,
          {coordinator, start_link, [self(), Route, Components]},
          temporary, 1000, worker, [coordinator]}
        ]}}.