#!/bin/bash

echo "compiling and running example $(pwd)"

folder=$1

mkdir -p $folder

echo "  compiling $folder"
cd $folder
cmake -DCMAKE_BUILD_TYPE=$folder -DOPENCMISS_BUILD_TYPE=$folder ..
make
cd ..
echo "  running $folder"
#mkdir -p results/current_run && ./$folder/src/example
mkdir -p results/current_run && ./$folder/src/example 160 120 120 6 6 6 1 0 10000 0.3 0.1
