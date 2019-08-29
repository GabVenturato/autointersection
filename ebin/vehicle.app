{application, vehicle,
  [{vsn, "1.0.0"},
   {description, "This application represents an Autonomous Vehicle (AV) capable
                  of solveing various situations: detect unresponsive 
                  (software failure) vehicles along its path, cross an 
                  intersection by cooperating with other AVs, stop on predefined
                  positions once they occur along its path and act accordingly."},
    {modules, [vehicle, av_supervisor, coordinator, component_supervisor,
               motion_event_handler, recognition_event_handler,
               communication_event_handler, motion_component,
               recognition_component, communication_component,
               mot_event_handler, com_event_handler, rec_event_handler]},
    {registered, [av_coord]},
    {mod, {vehicle, []}}
  ]}.