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
# <number elements X> <number elements Y> <interpolation type> <solver type> <PDE step size> <stop time> <output frequency> <CellML Model URL> <slow-twitch> <ODE time-step>

mkdir -p results/current_run/l1x1_n24x24_i1_s0 && ./$folder/src/example 24 24 1 0 0.05 0.5 1 n98.xml F 0.001
