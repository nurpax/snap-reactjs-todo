#!/bin/bash

die() { echo "$@" 1>&2 ; exit 1; }

# Run API tests

stack build || die "failed build"
stack exec server -- -e test > server-test.log 2>&1 &
serverPID=$!
# Wait for the server to start
stack exec server-api-test
kill -SIGINT $serverPID
