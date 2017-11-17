#!/bin/bash
# file: test.sh

stack test &
servant_pid=$!
sleep 1
# npm install
# npm run build
npm run test --prefix test/interface/golden

kill -9 $servant_pid
