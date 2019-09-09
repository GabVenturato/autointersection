#/bin/bash
  echo "RUNNING"
if [[ ( $# -lt 2 ) || ( $# -gt 7 ) ]]
then
	echo "command usage: start_generator.sh <vehicle number> <fail ratio> [relaive sw fail ratio] [max fail timeout (ms)] [host list] [node name] [cookie name]"
	exit 1
fi

if test $# -eq 2
then
	erl -sname env -pa ebin/ -noshell -eval "application:start(environment), vehicle_generator:start( $1, $2 )."
fi

if test $# -eq 3
then
	erl -sname env -pa ebin/ -noshell -eval "application:start(environment), vehicle_generator:start( $1, $2, $3 )."
fi

if test $# -eq 4
then
	erl -sname env -pa ebin/ -noshell -eval "application:start(environment), vehicle_generator:start( $1, $2, $3, $4 )."
fi

if test $# -eq 5
then
	erl -sname env -pa ebin/ -noshell -eval "application:start(environment), vehicle_generator:start( $1, $2, $3, $4, $5 )."
fi

if test $# -eq 7
then
	erl -name $6 -pa ebin/ -noshell -setcookie $7 -eval "application:start(environment), vehicle_generator:start( $1, $2, $3, $4, $5 )."
fi