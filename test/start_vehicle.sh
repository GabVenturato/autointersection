#/bin/bash

VEHICLE_EBIN=apps/vehicle/ebin/
ENVHOST=env@$HOSTNAME

if test $# -ne 3
then
	echo "command usage: initialize.sh <vehicle_name> <start_pos> <end_pos>"
	exit 1
fi

erl -sname v$1 -pa $VEHICLE_EBIN -eval \
"Route = rpc:call($ENVHOST, env, get_route, [\"$2\", \"$3\"]),
application:start(vehicle),
vehicle:initialize(Route, {env, $ENVHOST}),
vehicle:set_testing_environment({env, $ENVHOST}),
vehicle:startup()."
