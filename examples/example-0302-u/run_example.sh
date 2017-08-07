#!/bin/bash

echo "compiling and running example $(pwd)"

folder=$1
kind=$2

mkdir -p $folder

echo "  compiling $folder"
cd $folder
rm -rf CMakeCache.txt CMakeFiles
cmake -DCMAKE_BUILD_TYPE=$folder -DOPENCMISS_BUILD_TYPE=$folder ..
make
cd ..
echo "  running $folder"
echo "    2D tests"
mkdir -p results/current_run/2D_MeshRefinementLevel_001 && ./$folder/src/example 2 1 0.0 1.0 0.001 0.0025 1.0 0
mkdir -p results/current_run/2D_MeshRefinementLevel_002 && ./$folder/src/example 2 2 0.0 1.0 0.001 0.0025 1.0 0
mkdir -p results/current_run/2D_MeshRefinementLevel_003 && ./$folder/src/example 2 3 0.0 1.0 0.001 0.0025 1.0 0
mkdir -p results/current_run/2D_MeshRefinementLevel_001 && ./$folder/src/example 2 1 0.0 1.0 0.001 0.0025 1.0 1
mkdir -p results/current_run/2D_MeshRefinementLevel_002 && ./$folder/src/example 2 2 0.0 1.0 0.001 0.0025 1.0 1
mkdir -p results/current_run/2D_MeshRefinementLevel_003 && ./$folder/src/example 2 3 0.0 1.0 0.001 0.0025 1.0 1
if [ "$kind" == "big" ]; then
    echo "    3D tests"
    mkdir -p results/current_run/3D_MeshRefinementLevel_001 && ./$folder/src/example 3 1 0.0 1.0 0.001 0.01 1.0 0
    mkdir -p results/current_run/3D_MeshRefinementLevel_002 && ./$folder/src/example 3 2 0.0 1.0 0.001 0.01 1.0 0
    mkdir -p results/current_run/3D_MeshRefinementLevel_003 && ./$folder/src/example 3 3 0.0 1.0 0.001 0.01 1.0 0
    mkdir -p results/current_run/3D_MeshRefinementLevel_001 && ./$folder/src/example 3 1 0.0 1.0 0.001 0.01 1.0 1
    mkdir -p results/current_run/3D_MeshRefinementLevel_002 && ./$folder/src/example 3 2 0.0 1.0 0.001 0.01 1.0 1
    mkdir -p results/current_run/3D_MeshRefinementLevel_003 && ./$folder/src/example 3 3 0.0 1.0 0.001 0.01 1.0 1
fi
