#/bin/bash

NETWORK_NAME="autointersection_local"

# Stop all containers in the network $NETWORK_NAME.
docker stop $(docker network inspect \
  -f '{{ range $key, $value := .Containers }}{{ printf "%s\n" $key}}{{ end }}' \
  $NETWORK_NAME)