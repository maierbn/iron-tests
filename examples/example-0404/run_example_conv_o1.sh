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

mkdir -p results/current_run/l1x1_n1024_i2_s0_01 && ./$folder/src/example 1024 2 0 0.1 3.0 1 hodgkin_huxley_1952.cellml F 0.1
mkdir -p results/current_run/l1x1_n1024_i2_s0_05 && ./$folder/src/example 1024 2 0 0.05 3.0 1 hodgkin_huxley_1952.cellml F 0.05
mkdir -p results/current_run/l1x1_n1024_i2_s0_025 && ./$folder/src/example 1024 2 0 0.025 3.0 1 hodgkin_huxley_1952.cellml F 0.025
mkdir -p results/current_run/l1x1_n1024_i2_s0_01 && ./$folder/src/example 1024 2 0 0.01 3.0 1 hodgkin_huxley_1952.cellml F 0.01
mkdir -p results/current_run/l1x1_n1024_i2_s0_005 && ./$folder/src/example 1024 2 0 0.005 3.0 1 hodgkin_huxley_1952.cellml F 0.005
mkdir -p results/current_run/l1x1_n1024_i2_s0_001 && ./$folder/src/example 1024 2 0 0.001 3.0 1 hodgkin_huxley_1952.cellml F 0.001
mkdir -p results/current_run/l1x1_n1024_i2_s0_00025 && ./$folder/src/example 1024 2 0 0.00025 3.0 1 hodgkin_huxley_1952.cellml F 0.00025
