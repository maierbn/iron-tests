for directory in `ls current_run`
do
    echo $directory
    python2.7 ../src/scripts/plot3d.py current_run/$directory
    mv *.png ../doc/figures
    mv *.mp4 ../doc/figures
done

#python2.7 ../src/scripts/plot3d.py current_run/l1x1_n10x10_i1_s0_p1/
