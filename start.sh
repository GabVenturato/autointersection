#/bin/bash

# Script for starting docker tests.

if [[ ( $# -lt 2 ) || ( $# -gt 4 ) ]]
then
	echo "command usage: start.sh <vehicle number> \
                                <fail ratio> \
                                <relaive sw fail ratio> \
                                <max fail timeout (ms)>"
	exit 1
fi

# Store input parameters.

NUMBER_OF_VEHICLES=$1
FAIL_RATIO=$2
REL_SW_FAIL_RATIO=$3
MAX_FAIL_TIMEOUT=$4

NETWORK_NAME="autointersection_local"
COOKIE_NAME="test"

# Create user-defined network
docker network create --driver bridge $NETWORK_NAME

# Start node longname list.
LIST="["

BASE_NAME="v"

for I in $(seq 1 $NUMBER_OF_VEHICLES)
do

	SUFFIX=$I
  NAME="$BASE_NAME$SUFFIX"

  # Start vehicle container, attach it to the created network, create a volume for storing log files on host machine.
  docker run --name $NAME --network=$NETWORK_NAME -t -d --rm -v $(pwd)/docker_log:/autointersection/log erlang-autointersection:1.0

  # Take the ip address of the running container.
  IP_ADDR=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $NAME)

  # Create a longname (fully qualified name).
  NODENAME=$NAME@$IP_ADDR

  # Start the node $NODENAME in the container
  docker exec -d $NAME erl -pa ebin/ -noshell -name $NODENAME -setcookie $COOKIE_NAME

  # Append the generated node name to the node list
  LIST="$LIST'$NODENAME',"

done

# Remove the last ',' and close the list
NODE_LIST="${LIST::-1}]"

# Start main container (for the environment and generator) and attach it to the network.
docker run --name main --network=$NETWORK_NAME -t -d --rm erlang-autointersection:1.0

IP_ADDR=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' main)

MAIN="env@$IP_ADDR"

# Start the generator on the main container.
docker exec -it main /bin/bash test/start_generator.sh $NUMBER_OF_VEHICLES \
                                                       $FAIL_RATIO \
                                                       $REL_SW_FAIL_RATIO \
                                                       $MAX_FAIL_TIMEOUT \
                                                       $NODE_LIST \
                                                       $MAIN \
                                                       $COOKIE_NAME