#!/bin/bash -x

echo "This script will be used for testing / triggering Jenkins / showing a test summary from Jenkins test"

echo "Should also test serial / parallel execution!"

here=$(pwd)
for examplefolder in `ls -d example-*`
do
    cd $examplefolder
    make debug
    make release
    cd $here
done
pwd
