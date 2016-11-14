#!/bin/bash

# Run API tests

stack build
stack exec snap-reactjs-todo -- -e test > server-test.log 2>&1 &
serverPID=$!
# Wait for the server to start
stack exec server-api-test
kill -SIGINT $serverPID
