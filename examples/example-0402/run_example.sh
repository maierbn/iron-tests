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
# <number elements X> <number elements Y> <interpolation type> <solver type> <PDE step size> <stop time> <output frequency> <CellML Model URL>
mkdir -p results/current_run/l1x1_n10x10_i1_s0 && ./$folder/src/example 10 10 1 0 0.05 1.0 10 n98.xml
