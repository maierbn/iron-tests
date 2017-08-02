#!/bin/bash

here=$(pwd)
for examplefolder in `ls -d example-*`
do
    cd $examplefolder
    make create-figures
    cd $here
done
