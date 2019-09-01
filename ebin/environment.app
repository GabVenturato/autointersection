{application, environment,
  [{vsn, "1.0.0"},
   {description, "Substitute for internal sensors of an Autonomous Vehicle.
                  Environment information (roads and intersections) are kept
                  in a directed graph, vehicles can ask only for information
                  they would otherwise obtain from their internal sensors."},
    {modules, [env_supervisor]},
    {registered, []},
    {mod, {, []}}
  ]}.