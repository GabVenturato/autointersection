#/bin/bash

# Script that cleans the host machine (docker environment and project dir) from
# the previous tests run with ./start.sh.

NETWORK_NAME="autointersection_local"

# Stop all containers in the network $NETWORK_NAME.
docker stop $(docker network inspect \
  -f '{{ range $key, $value := .Containers }}{{ printf "%s\n" $key}}{{ end }}' \
  $NETWORK_NAME)

docker network rm $NETWORK_NAME

# Remove log folder.
rm -r $(pwd)/docker_log