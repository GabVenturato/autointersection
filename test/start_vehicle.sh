#/bin/bash

if test $# -ne 1
then
	echo "command usage: initialize.sh <vehicle_name>"
	exit 1
fi

erl -sname v$1 -pa ../ebin/ -eval "net_kernel:connect_node(environment@$HOSTNAME), intersection_coordination:start( {environment, environment@$HOSTNAME}, null )."