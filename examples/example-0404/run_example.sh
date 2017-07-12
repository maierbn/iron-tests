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

#mkdir -p results/current_run/l1x1_n16_i2_s0_05 && ./$folder/src/example 16 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n16_i2_s0_05
#mkdir -p results/current_run/l1x1_n10_i2_s0_05 && ./$folder/src/example 10 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n10_i2_s0_05
#mkdir -p results/current_run/l1x1_n8_i2_s0_05 && ./$folder/src/example 8 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n8_i2_s0_05
#mkdir -p results/current_run/l1x1_n6_i2_s0_05 && ./$folder/src/example 6 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n6_i2_s0_05
#mkdir -p results/current_run/l1x1_n4_i2_s0_05 && ./$folder/src/example 4 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n4_i2_s0_05
#mkdir -p results/current_run/l1x1_n48_i2_s0_05 && ./$folder/src/example 48 2 0 0.05 10 1 hodgkin_huxley_1952.cellml T 0.05 && mv *.ex* results/current_run/l1x1_n48_i2_s0_05
#mkdir -p results/current_run/l1x1_n48_i2_s0_05 && ./$folder/src/example 48 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n48_i2_s0_05
#mkdir -p results/current_run/l1x1_n50_i2_s0_05 && ./$folder/src/example 50 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n50_i2_s0_05
mkdir -p results/current_run/l1x1_n64_i2_s0_05 && ./$folder/src/example 64 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n64_i2_s0_05
#instable
#mkdir -p results/current_run/l1x1_n96_i2_s0_05 && ./$folder/src/example 96 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n96_i2_s0_05
#mkdir -p results/current_run/l1x1_n72_i2_s0_05 && ./$folder/src/example 72 2 0 0.05 10 1 hodgkin_huxley_1952.cellml F 0.05 && mv *.ex* results/current_run/l1x1_n72_i2_s0_05

