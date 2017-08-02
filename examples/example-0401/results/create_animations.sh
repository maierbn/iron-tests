for directory in `ls current_run`
do
    echo $directory
    python2.7 ../src/scripts/plot3d.py current_run/$directory
    mv *.eps ../doc/figures/
    mv *.png ../doc/figures/
    mv *.mp4 ../doc/figures/
done

