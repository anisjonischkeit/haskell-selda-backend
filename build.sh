#!/bin/bash

set -e

docker exec -t haskell-selda-backend_backend-builder_1 /bin/bash -c "stack build --copy-bins"
docker exec -t haskell-selda-backend_backend_1 /bin/bash -c "supervisorctl restart backend"
