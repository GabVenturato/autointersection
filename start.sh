#/bin/bash

if [[ ( $# -lt 2 ) || ( $# -gt 4 ) ]]
then
	echo "command usage: start.sh <vehicle number> <fail ratio> [relaive sw fail ratio] [max fail timeout (ms)]"
	exit 1
fi

NETWORK_NAME="autointersection_local"

export NUMBER_OF_VEHICLES=$1
export FAIL_RATIO=$2
export REL_SW_FAIL_RATIO=$3
export MAX_FAIL_TIMEOUT=$4

docker network create --driver bridge $NETWORK_NAME || true

# Random string of lowercase letters and numbers as a base for the hostname.
BASE=$(cat /dev/urandom | tr -dc 'a-z0-9' | fold -w 8 | head -n 1)

# Start hostname list.
LIST="\"["

for I in $(seq 1 $NUMBER_OF_VEHICLES)
do
  # Add suffix to BASE to create a unique hostname.
	SUFFIX=$I
  HOSTNAME="$BASE$SUFFIX"
echo $HOSTNAME

  # Append the generated hostname to the hostname list
  LIST="$LIST$HOSTNAME,"

  docker run -h $HOSTNAME --network=$NETWORK_NAME -t -d --rm erlang-autointersection:1.0
done

# Remove the last ',' and close the list
export HOSTNAME_LIST="${LIST::-1}]\""

echo $HOSTNAME_LIST
docker-compose up --force-recreate