{application, vehicle,
  [{vsn, "1.0.0"},
   {description, "This application represents an Autonomous Vehicle (AV) capable
                  of solving various situations: detect unresponsive 
                  (software failure) vehicles along its path, cross an 
                  intersection by cooperating with other AVs, stop on predefined
                  positions once they occur along its path and act accordingly."},
    {modules, [vehicle, vehicle_supervisor, coordinator, component_supervisor,
               action_supervisor, motion_event_handler, recognition_event_handler,
               communication_event_handler, motion_component,
               recognition_component, communication_component,
               mot_event_handler, com_event_handler, rec_event_handler,
               av_intersection_coordination]},
    {registered, []},
    {mod, {vehicle, []}}
  ]}.