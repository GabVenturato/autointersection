-module(vehicle).
-behavior(gen_statem).

-export([start/0, start_link/0, callback_mode/0]).
-export([init/1]).

-record( state,
  { path
  , position
  }).

%%% -------------------------- Interface Functions ------------------------- %%%

start() ->
  gen_statem:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).


callback_mode() ->
  state_functions.

%%% -------------------------- Callback Functions -------------------------- %%%

init([]) ->
  State = #state{},
  {ok, waiting, State}.

%%% STATE: WAITING