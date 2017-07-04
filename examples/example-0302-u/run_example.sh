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
echo "    2D tests"
mkdir -p results/current_run/2D_MeshRefinementLevel_001 && ./$folder/src/example 2 1 0.0 0.01 0.001 1.0 0.0025 0
