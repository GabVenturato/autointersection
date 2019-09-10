#/bin/bash

# Script that cleans the host machine (docker environment and project dir) from
# the previous tests run with 'start_docker.sh'.

NETWORK_NAME="autointersection_local"

# Stop all containers in the network $NETWORK_NAME.
docker/stop_docker.sh

docker network rm $NETWORK_NAME

# Remove log directory
rm -rf docker_log