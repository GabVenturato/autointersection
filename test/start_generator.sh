#/bin/bash

ENVIRONMENT_EBIN=apps/environment/ebin/
VEHICLE_EBIN=apps/vehicle/ebin/
GENERATOR_EBIN=test/ebin/

EBIN="$ENVIRONMENT_EBIN $VEHICLE_EBIN $GENERATOR_EBIN"

printf "\nRUNNING GENERATOR\n"

if [[ ( $# -lt 2 ) || ( $# -gt 8 ) || ( $# -eq 6 ) ]]
then
	echo "command usage: start_generator.sh <vehicle number> <fail ratio> [relaive sw fail ratio] [max fail timeout (ms)] [node list] [name type] [node name] [cookie name]"
	exit 1
fi

# Tests with shortnames:
if test $# -eq 2
then
	erl -sname env -pa $EBIN -noshell -eval "application:start(environment), vehicle_generator:start( $1, $2 )."
fi

if test $# -eq 3
then
	erl -sname env -pa $EBIN -noshell -eval "application:start(environment), vehicle_generator:start( $1, $2, $3 )."
fi

if test $# -eq 4
then
	erl -sname env -pa $EBIN -noshell -eval "application:start(environment), vehicle_generator:start( $1, $2, $3, $4 )."
fi

if test $# -eq 5
then
	erl -sname env -pa $EBIN -noshell -eval "application:start(environment), vehicle_generator:start( $1, $2, $3, $4, $5 )."
fi

# Test with longnames: $6 must be 'longnames', $7 must be the env longname:
if test $# -eq 7
then
	erl -name $7 -pa $EBIN -noshell -eval "application:start(environment), vehicle_generator:start( $1, $2, $3, $4, $5, $6 )."
fi

# Test (used by docker) with longnames and cookie specification:
if test $# -eq 8
then
	erl -name $7 -pa $EBIN -noshell -setcookie $8 -eval "application:start(environment), vehicle_generator:start( $1, $2, $3, $4, $5, $6 )."
fi
