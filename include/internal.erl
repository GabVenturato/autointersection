-module(internal).

-export([notify/2, log/2, a_log/4, event/4]).

-include("../include/event.hrl").

%% Generic synchronous event notification.
notify(Pid, Event) ->
    %io:format("Sending: ~p~n", [Event]),
  gen_event:notify(Pid, Event).

log(Pid, Msg) ->
  a_log(Pid, [], "Vehicle internal", Msg).

a_log(Pid, [], Tag, Msg) ->
  Event = #event{type = {log, info}, name = Tag, content = Msg},
  notify(Pid, Event);

a_log(Pid, Level, Tag, Msg) ->
  Event = #event{type = {log, Level}, name = Tag, content = Msg},
  notify(Pid, Event).

event(Pid, Type, Name, []) ->
  event(Pid, Type, Name, none);

event(Pid, Type, Name, Content) ->
  Event = #event{type = Type, name = Name, content = Content},
  notify(Pid, Event).