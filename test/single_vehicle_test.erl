-module(single_vehicle_test).

-behavior(gen_server).

% Exports the required gen_server callbacks.
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% Export startup functions.
-export([start/0, start_link/0, start_test/0]).

start_test() ->
  {ok, Pid} = start_link(),
  vehicle:start_test(get_route(), Pid).

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call({position_status, _Position}, _From, State) ->
  {reply, random_status(), State};

handle_call({is_position_free, _Position}, _From, State) ->
  {Send, NewState} = is_position_free(State),
  {reply, Send, NewState};

handle_call({object_at, _Position}, _From, _State) ->

  {reply, random_object(), wait};
  
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({solve_intersection, EvManPid}, State) ->
  gen_event:notify(EvManPid, {coo, {position_status, solve_intersection()}}),
  {noreply, State};

handle_cast({vehicle_down, _}, _State) ->
  io:format("ENV: vehicle down msg received. ~n"),
  {noreply, switch};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions


get_route() ->
  lists:duplicate(1000, a).

random_status() ->
  Number = rand:uniform(100),
  if 
  Number >= 90 ->
    stop;
  true ->
    free
  end.

is_position_free(State) ->
  case State of
    switch -> 
      {true, true};
    wait ->
      {false, wait};
    _other ->
    Number = rand:uniform(100),
    if 
    Number > 70 ->
      {false, false};
    true ->
      {true, true}
    end
  end.

solve_intersection() ->
  Number = rand:uniform(1800),
  timer:sleep(3000-Number),
  free.

random_object() -> 
  Number = rand:uniform(100),
  if 
  Number > 90 ->
    % Dead vehicle.
    {vehicle, spawn(fun() -> exit(boom) end)};
  true ->
    {vehicle, spawn(fun() -> timer:sleep(8000 - rand:uniform(2000)), exit(boom) end)}
  end.


