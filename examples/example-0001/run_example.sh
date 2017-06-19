#!/bin/bash

echo "compiling and running example $(pwd)"

folders="debug release"

mkdir -p $folders

for folder in $folders
do
    echo "  compiling $folder"
    cd $folder
    cmake -DCMAKE_BUILD_TYPE=$folder -DOPENCMISS_BUILD_TYPE=$folder ..
    make
    cd ..
    echo "  running $folder"
    ./$folder/Fortran/example
done
