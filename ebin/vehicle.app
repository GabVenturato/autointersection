{application, vehicle,
  [{vsn, "1.0.0"},
   {description, "This application represents an Autonomous Vehicle (AV) capable
                  of solveing various situations: detect unresponsive 
                  (software failure) vehicles along its path, cross an 
                  intersection by cooperating with other AVs, stop on predefined
                  positions once they occur along its path and act accordingly."},
    {modules, [vehicle, av_supervisor, av_coordinator, av_component_supervisor,
               av_motion_event_handler, av_recognition_event_handler,
               av_communication_event_handler, av_motion_component,
               av_recognition_component, av_communication_component,
               mot_event_handler, com_event_handler, rec_event_handler]},
    {registered, [av_coord]},
    {mod, {vehicle, []}}
  ]}.