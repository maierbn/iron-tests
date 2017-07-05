#!/bin/bash
# test cases for example-0401 (2D problem with Hodgkin-Huxley)

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

mkdir -p results/current_run/l1x1_n24x24_i1_s0_p1 && ./$folder/src/example 24 24 1 0 0.005 3.0 1 hodgkin_huxley_1952.cellml F 0.0001 && mv *.ex* results/current_run/l1x1_n24x24_i1_s0_p1
mkdir -p results/current_run/l1x1_n24x24_i1_s1_p1 && ./$folder/src/example 24 24 1 0 0.005 3.0 1 hodgkin_huxley_1952.cellml F 0.005  && mv *.ex* results/current_run/l1x1_n24x24_i1_s1_p1
mkdir -p results/current_run/l1x1_n10x10_i1_s0_p1 && ./$folder/src/example 10 10 1 0 0.005 3.0 1 hodgkin_huxley_1952.cellml F 0.0001 && mv *.ex* results/current_run/l1x1_n10x10_i1_s0_p1
#mkdir -p results/current_run/l1x1_n24x24_i1_s0_p2 && mpirun -n 2 ./$folder/src/example 24 24 1 0 0.005 3.0 1 hodgkin_huxley_1952.cellml F 0.0001 && mv *.ex* results/current_run/l1x1_n24x24_i1_s0_p2
#mkdir -p results/current_run/l1x1_n24x24_i1_s0_p8 && mpirun -n 8 ./$folder/src/example 24 24 1 0 0.005 3.0 1 hodgkin_huxley_1952.cellml F 0.0001 && mv *.ex* results/current_run/l1x1_n24x24_i1_s0_p8
#mkdir -p results/current_run/l1x1_n2x2_i1_s0_p1 && ./$folder/src/example 2 2 1 0 0.005 3.0 1 hodgkin_huxley_1952.cellml F 0.0001 && mv *.ex* results/current_run/l1x1_n2x2_i1_s0_p1
#mkdir -p results/current_run/l1x1_n2x2_i1_s0_p2 && mpirun -n 2 ./$folder/src/example 2 2 1 0 0.005 3.0 1 hodgkin_huxley_1952.cellml F 0.0001 && mv *.ex* results/current_run/l1x1_n2x2_i1_s0_p2
