#!/bin/bash
# test cases for example-0404 (1D problem with Hodgkin-Huxley)

echo "compiling and running example $(pwd)"

folder=$1

mkdir -p $folder

echo "  compiling $folder"
cd $folder
cmake -DCMAKE_BUILD_TYPE=$folder -DOPENCMISS_BUILD_TYPE=$folder ..
make
cd ..
echo "  running $folder"
# <number elements X> <interpolation type> <solver type> <PDE step size> <stop time> <output frequency> <CellML Model URL> <slow-twitch> <ODE time-step>

mkdir -p results/current_run/l1x1_n16_i2_s0_01 && ./$folder/src/example 16 2 0 0.01 10 1 hodgkin_huxley_1952.cellml F 0.01 && mv *.ex* results/current_run/l1x1_n16_i2_s0_01

