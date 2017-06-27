gfx read node Monodomain_24x24.part0.exnode;
gfx read element Monodomain_24x24.part0.exelem; 
gfx read node Time_2_0.part0.exnode
gfx read node Time_2_10.part0.exnode time 2
gfx read node Time_2_20.part0.exnode time 3
gfx read node Time_2_30.part0.exnode time 4
gfx read node Time_2_40.part0.exnode time 5
gfx read node Time_2_50.part0.exnode time 6

#@exnodes=<./Time_*.part*.exnode>;
#$time_index=1;
#foreach $filename (@exnodes) {
#    print "Reading $filename\n";
#    gfx read node "$filename" time $time_index;
#    $time_index++;
#}
gfx define faces egroup Region;
gfx create window 1;
gfx modify window 1 background colour 1.0 1.0 1.0;
gfx modify window 1 view interest_point 0.5,0.5,0.0 eye_point 0.5,0.5,3.0 up_vector 0.0,1.0,0.0;
gfx modify spectrum default clear overwrite_colour;
gfx modify spectrum default linear reverse range -95.0 50.0 extend_above extend_below rainbow colour_range 0 1 component 1;
gfx modify spectrum default linear reverse range -95.0 50.0 banded number_of_bands 10 band_ratio 0.05 component 1;
gfx modify g_element Region lines material black;
gfx modify g_element Region surfaces select_on coordinate Coordinate material default data Vm spectrum default selected_material default_selected render_shaded;
gfx print jpg window 1 file gNa.jpg;
gfx print postscript window 1 file gNa.ps;
