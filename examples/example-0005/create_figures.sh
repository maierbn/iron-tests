#!/bin/bash

for cmguifile in `ls results/*.com`
do
    # run cmgui script file and export screenshot
    echo $cmguifile
    cmgui $cmguifile &
    sleep 2
    # close cmgui window by using PID of last command
    kill `echo $!`
done
