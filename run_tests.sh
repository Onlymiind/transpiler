#!/bin/bash

#builds and runs tests

CONFIG="Release"
if [ $# -gt 0 ];
then
    CONFIG=$1
fi

mkdir "-p" build_tests && cd build_tests
cmake "-S" ".." "-DCMAKE_BUILD_TYPE=$CONFIG" && cmake "--build" "." "--config" $CONFIG
cd "../tests/functional" && "../../build_tests/tests"
