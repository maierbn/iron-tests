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

#mkdir -p results/current_run/l1x1_n10_i1_s0_05 && ./$folder/src/example 10 1 0 0.05 3.0 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n10_i1_s0_05
mkdir -p results/current_run/l1x1_n10_i2_s0_05 && ./$folder/src/example 10 2 0 0.05 3.0 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n10_i2_s0_05
