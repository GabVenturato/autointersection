#/bin/bash

if [[ ( $# -lt 2 ) || ( $# -gt 5 ) ]]
then
	echo "command usage: start_generator.sh <vehicle number> <fail ratio> [relaive sw fail ratio] [max fail timeout (ms)] [host list]"
	exit 1
fi

if test $# -eq 2
then
	erl -sname env -pa ebin/ -eval "application:start(environment), vehicle_generator:start( $1, $2 )."
fi

if test $# -eq 3
then
	erl -sname env -pa ebin/ -eval "application:start(environment), vehicle_generator:start( $1, $2, $3 )."
fi

if test $# -eq 4
then
	erl -sname env -pa ebin/ -eval "application:start(environment), vehicle_generator:start( $1, $2, $3, $4 )."
fi

if test $# -eq 5
then
	erl -sname env -pa ebin/ -eval "application:start(environment), vehicle_generator:start( $1, $2, $3, $4, $5 )."
fi