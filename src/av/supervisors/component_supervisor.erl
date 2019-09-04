%%% Supervisor module in charge of supervising various AV's components.

-module(component_supervisor).

-behavior(supervisor).

-export([start_link/1]).

-export([init/1]).

% Supervisor configuration details
-define(RESTART_STRATEGY, one_for_one).
-define(MAX_RESTART, 0).
-define(MAX_TIME, 60).

%%% -------------------------- Interface Functions ------------------------- %%%

start_link(Config) ->
  supervisor:start_link({local,?MODULE}, ?MODULE, Config).

%%% -------------------------- Callback Functions -------------------------- %%%

init([]) ->
  {ok, {{?RESTART_STRATEGY, ?MAX_RESTART, ?MAX_TIME}, []}};

init([Restart_strategy, Max_restart, Max_time]) ->
  {ok, {{Restart_strategy, Max_restart, Max_time}, []}}.