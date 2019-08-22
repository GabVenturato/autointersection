-module(intersection_gen).
-compile(export_all).
-record(intersection,
  { ins = []
  , outs = []
  , graph
  }).

-define(INTERSECTION_CONF_FILE, "../conf/intersection.conf").

init_intersection() ->
  {ok, Content} = file:consult(?INTERSECTION_CONF_FILE),
  Env = #intersection{ graph = digraph:new() },
  Env1 = build_graph(Env, Content),
  io:format("Ins: ~p~nOuts: ~p~nNodes: ~p~n", [
    Env1#intersection.ins,
    Env1#intersection.outs,
    digraph:vertices(Env1#intersection.graph)
  ]),
  In = hd(Env1#intersection.ins),
  Out = hd(Env1#intersection.outs),
  digraph:get_short_path(Env1#intersection.graph, In, Out).

build_graph(Env, Content) ->
  Env1 = add_nodes(Env, Content),
  add_edges(Env1, Content).

add_nodes(Env, [Elem|Content]) ->
  case Elem of
    {edge, _, _} -> add_nodes(Env, Content); % ignore edges
    {V, Node} -> 
      Env1 = case V of
        in  -> Env#intersection{ ins  = [ Node | Env#intersection.ins ] };
        out -> Env#intersection{ outs = [ Node | Env#intersection.outs ] };
        _   -> Env
      end,
      G = Env1#intersection.graph,
      digraph:add_vertex( G, Node ),
      Env2 = Env1#intersection{ graph = G },
      add_nodes(Env2, Content)
  end;
add_nodes(Env, []) -> Env.

add_edges(Env, [Elem|Content]) ->
  case Elem of
    {edge, Node1, Node2} -> 
      G = Env#intersection.graph,
      digraph:add_edge( G, Node1, Node2 ),
      Env1 = Env#intersection{ graph = G },
      add_edges(Env1, Content);
    _ -> add_edges(Env, Content) % ignore nodes
  end;
add_edges(Env, []) -> Env.