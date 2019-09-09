#/bin/bash

if [[ ( $# -lt 2 ) || ( $# -gt 4 ) ]]
then
	echo "command usage: start.sh <vehicle number> <fail ratio> [relaive sw fail ratio] [max fail timeout (ms)]"
	exit 1
fi



NETWORK_NAME="autointersection_local"
COOKIE_NAME="test"

NUMBER_OF_VEHICLES=$1
FAIL_RATIO=$2
REL_SW_FAIL_RATIO=$3
MAX_FAIL_TIMEOUT=$4

docker network create --driver bridge $NETWORK_NAME


# Start node longname list.
LIST="["

BASE_NAME="v"

for I in $(seq 1 $NUMBER_OF_VEHICLES)
do

	SUFFIX=$I
  NAME="$BASE_NAME$SUFFIX"

  docker run --name $NAME --network=$NETWORK_NAME -t -d --rm erlang-autointersection:1.0
  IP_ADDR=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $NAME)

  NODENAME=$NAME@$IP_ADDR

  docker exec -d $NAME erl -pa ebin/ -noshell -name $NODENAME -setcookie $COOKIE_NAME

  # Append the generated hostname to the hostname list
  LIST="$LIST'$NODENAME',"

done

# Remove the last ',' and close the list
NODENAME_LIST="${LIST::-1}]"

docker run --name main --network=$NETWORK_NAME -t -d --rm erlang-autointersection:1.0

IP_ADDR=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' main)

MAIN="env@$IP_ADDR"

docker exec -it main /bin/bash test/start_generator.sh $NUMBER_OF_VEHICLES $FAIL_RATIO $REL_SW_FAIL_RATIO $MAX_FAIL_TIMEOUT $NODENAME_LIST $MAIN $COOKIE_NAME