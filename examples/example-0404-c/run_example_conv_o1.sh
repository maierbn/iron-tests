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

mkdir -p results/current_run/l1x1_n1024_i1_s0_01 && ./$folder/src/example 1024 1 0 0.01 3.0 1 hodgkin_huxley_1952.cellml F 0.01 && mv *.ex* results/current_run/l1x1_n1024_i2_s0_01
mkdir -p results/current_run/l1x1_n1024_i1_s0_005 && ./$folder/src/example 1024 1 0 0.005 3.0 1 hodgkin_huxley_1952.cellml F 0.005 && mv *.ex* results/current_run/l1x1_n1024_i2_s0_005
mkdir -p results/current_run/l1x1_n1024_i1_s0_001 && ./$folder/src/example 1024 1 0 0.001 3.0 1 hodgkin_huxley_1952.cellml F 0.001 && mv *.ex* results/current_run/l1x1_n1024_i2_s0_001
mkdir -p results/current_run/l1x1_n64_i2_s0_0005 && ./$folder/src/example 64 2 0 0.0005 10 100 hodgkin_huxley_1952.cellml F 0.0005 && mv *.ex* results/current_run/l1x1_n64_i2_s0_0005
mkdir -p results/current_run/l1x1_n1024_i1_s0_00025 && ./$folder/src/example 1024 1 0 0.00025 3.0 1 hodgkin_huxley_1952.cellml F 0.00025 && mv *.ex* results/current_run/l1x1_n1024_i2_s0_00025
mkdir -p results/current_run/l1x1_n64_i2_s0_0001 && ./$folder/src/example 64 2 0 0.0001 10 200 hodgkin_huxley_1952.cellml F 0.0001 && mv *.ex* results/current_run/l1x1_n64_i2_s0_0001
mkdir -p results/current_run/l1x1_n64_i2_s0_00005 && ./$folder/src/example 64 2 0 0.00005 10 200 hodgkin_huxley_1952.cellml F 0.00005 && mv *.ex* results/current_run/l1x1_n64_i2_s0_00005
