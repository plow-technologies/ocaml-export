#!/bin/bash
# file: test.sh

stack build
stack test --no-run-tests 
stack test &
servant_pid=$!
sleep 5

# we store the lib so no need to build
npm run test --prefix test/interface/golden

kill -9 $servant_pid
