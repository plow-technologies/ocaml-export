#!/bin/bash
# file: test.sh

stack test &
servant_pid=$!
sleep 1

# npm install --prefix test/interface/golden
# npm run build --prefix test/interface/golden
npm run test --prefix test/interface/golden

kill -9 $servant_pid
