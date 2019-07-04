#!/bin/bash

docker exec -it haskell-selda-backend_postgres_1 \
    psql -h postgres -U postgres -d db1