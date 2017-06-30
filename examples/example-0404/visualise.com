# automatically generated at 2017-06-28 14:22
# nodes
gfx read node Time_2_0.part0.exnode time 0
gfx read node Time_2_1.part0.exnode time 1
gfx read node Time_2_2.part0.exnode time 2
gfx read node Time_2_3.part0.exnode time 3
gfx read node Time_2_4.part0.exnode time 4
gfx read node Time_2_5.part0.exnode time 5
gfx read node Time_2_6.part0.exnode time 6
gfx read node Time_2_7.part0.exnode time 7
gfx read node Time_2_8.part0.exnode time 8
gfx read node Time_2_9.part0.exnode time 9
gfx read node Time_2_10.part0.exnode time 10
gfx read node Time_2_11.part0.exnode time 11
gfx read node Time_2_12.part0.exnode time 12
gfx read node Time_2_13.part0.exnode time 13
gfx read node Time_2_14.part0.exnode time 14
gfx read node Time_2_15.part0.exnode time 15
gfx read node Time_2_16.part0.exnode time 16
gfx read node Time_2_17.part0.exnode time 17
gfx read node Time_2_18.part0.exnode time 18
gfx read node Time_2_19.part0.exnode time 19
gfx read node Time_2_20.part0.exnode time 20
gfx read node Time_2_21.part0.exnode time 21
gfx read node Time_2_22.part0.exnode time 22
gfx read node Time_2_23.part0.exnode time 23
gfx read node Time_2_24.part0.exnode time 24
gfx read node Time_2_25.part0.exnode time 25
gfx read node Time_2_26.part0.exnode time 26
gfx read node Time_2_27.part0.exnode time 27
gfx read node Time_2_28.part0.exnode time 28
gfx read node Time_2_29.part0.exnode time 29
gfx read node Time_2_30.part0.exnode time 30
gfx read node Time_2_31.part0.exnode time 31
gfx read node Time_2_32.part0.exnode time 32
gfx read node Time_2_33.part0.exnode time 33
gfx read node Time_2_34.part0.exnode time 34
gfx read node Time_2_35.part0.exnode time 35
gfx read node Time_2_36.part0.exnode time 36
gfx read node Time_2_37.part0.exnode time 37
gfx read node Time_2_38.part0.exnode time 38
gfx read node Time_2_39.part0.exnode time 39
gfx read node Time_2_40.part0.exnode time 40
gfx read node Time_2_41.part0.exnode time 41
gfx read node Time_2_42.part0.exnode time 42
gfx read node Time_2_43.part0.exnode time 43
gfx read node Time_2_44.part0.exnode time 44
gfx read node Time_2_45.part0.exnode time 45
gfx read node Time_2_46.part0.exnode time 46
gfx read node Time_2_47.part0.exnode time 47
gfx read node Time_2_48.part0.exnode time 48
gfx read node Time_2_49.part0.exnode time 49
gfx read node Time_2_50.part0.exnode time 50
gfx read node Time_2_51.part0.exnode time 51
gfx read node Time_2_52.part0.exnode time 52
gfx read node Time_2_53.part0.exnode time 53
gfx read node Time_2_54.part0.exnode time 54
gfx read node Time_2_55.part0.exnode time 55
gfx read node Time_2_56.part0.exnode time 56
gfx read node Time_2_57.part0.exnode time 57
gfx read node Time_2_58.part0.exnode time 58
gfx read node Time_2_59.part0.exnode time 59
gfx read node Time_2_60.part0.exnode time 60

# elements

gfx read element Monodomain_2x2.part0.exelem
# ----------------------



gfx modify spectrum default clear overwrite_colour;
gfx modify spectrum default linear reverse range -95 100 extend_above extend_below rainbow colour_range 0 1 component 1;
gfx modify spectrum default linear reverse range -95 50 banded number_of_bands 10 band_ratio 0.05 component 1;
gfx create material black normal_mode ambient 0 0 0 diffuse 0 0 0 emission 0 0 0 specular 0.3 0.3 0.3 alpha 1 shininess 0.2;
gfx create material blue normal_mode ambient 0 0 0.5 diffuse 0 0 1 emission 0 0 0 specular 0.2 0.2 0.2 alpha 1 shininess 0.2;
gfx create material bone normal_mode ambient 0.7 0.7 0.6 diffuse 0.9 0.9 0.7 emission 0 0 0 specular 0.1 0.1 0.1 alpha 1 shininess 0.2;
gfx create material default normal_mode ambient 1 1 1 diffuse 1 1 1 emission 0 0 0 specular 0 0 0 alpha 1 shininess 0;
gfx create material default_selected normal_mode ambient 1 0.2 0 diffuse 1 0.2 0 emission 0 0 0 specular 0 0 0 alpha 1 shininess 0;
gfx create material gold normal_mode ambient 1 0.4 0 diffuse 1 0.7 0 emission 0 0 0 specular 0.5 0.5 0.5 alpha 1 shininess 0.3;
gfx create material gray50 normal_mode ambient 0.5 0.5 0.5 diffuse 0.5 0.5 0.5 emission 0.5 0.5 0.5 specular 0.5 0.5 0.5 alpha 1 shininess 0.2;
gfx create material green normal_mode ambient 0 0.5 0 diffuse 0 1 0 emission 0 0 0 specular 0.2 0.2 0.2 alpha 1 shininess 0.1;
gfx create material muscle normal_mode ambient 0.4 0.14 0.11 diffuse 0.5 0.12 0.1 emission 0 0 0 specular 0.3 0.5 0.5 alpha 1 shininess 0.2;
gfx create material red normal_mode ambient 0.5 0 0 diffuse 1 0 0 emission 0 0 0 specular 0.2 0.2 0.2 alpha 1 shininess 0.2;
gfx create material silver normal_mode ambient 0.4 0.4 0.4 diffuse 0.7 0.7 0.7 emission 0 0 0 specular 0.5 0.5 0.5 alpha 1 shininess 0.3;
gfx create material tissue normal_mode ambient 0.9 0.7 0.5 diffuse 0.9 0.7 0.5 emission 0 0 0 specular 0.2 0.2 0.3 alpha 1 shininess 0.2;
gfx create material transparent_gray50 normal_mode ambient 0.5 0.5 0.5 diffuse 0.5 0.5 0.5 emission 0.5 0.5 0.5 specular 0.5 0.5 0.5 alpha 0 shininess 0.2;
gfx create material white normal_mode ambient 1 1 1 diffuse 1 1 1 emission 0 0 0 specular 0 0 0 alpha 1 shininess 0;
gfx modify g_element Region general clear circle_discretization 6 default_coordinate Coordinate element_discretization "4*4*4" native_discretization none;
gfx modify g_element Region lines select_on material black selected_material default_selected;
gfx modify g_element Region surfaces coordinate Coordinate select_on material default data Vm spectrum default selected_material default_selected render_shaded;
gfx modify g_element Region node_points glyph sphere general size "0.1*0.1*0.1" centre 0,0,0 font default select_on material default data Vm spectrum default selected_material default_selected;
gfx create window 1 double_buffer;
gfx modify window 1 image scene default light_model default;
gfx modify window 1 image add_light default;
gfx modify window 1 layout simple ortho_axes z -y eye_spacing 0.25 width 512 height 512;
gfx modify window 1 set current_pane 1;
gfx modify window 1 background colour 1 1 1 texture none;
gfx modify window 1 view parallel eye_point 0.429694 0.48238 2.99912 interest_point 0.5 0.5 0 up_vector 0.000377121 0.999983 0.00588394 view_angle 40 near_clipping_plane 0.0288485 far_clipping_plane 10.3095 relative_viewport ndc_placement -1 1 2 2 viewport_coordinates 0 0 1 1;
gfx modify window 1 overlay scene none;
gfx modify window 1 set transform_tool current_pane 1 std_view_angle 40 normal_lines no_antialias depth_of_field 0.0 fast_transparency blend_normal;



#gfx define faces egroup Region;
#gfx create window 1;
#gfx modify window 1 background colour 1.0 1.0 1.0;
#gfx modify window 1 view interest_point 0.5,0.5,0.0 eye_point 0.5,0.5,3.0 up_vector 0.0,1.0,0.0;
#gfx modify spectrum default clear overwrite_colour;
#gfx modify spectrum default linear reverse range -95.0 50.0 extend_above extend_below rainbow colour_range 0 1 component 1;
#gfx modify spectrum default linear reverse range -95.0 50.0 banded number_of_bands 10 band_ratio 0.05 component 1;
#gfx modify g_element Region lines material black;
#gfx modify g_element Region surfaces select_on coordinate Coordinate material default data Vm spectrum default selected_material default_selected render_shaded;
#gfx print jpg window 1 file gNa.jpg;
#gfx print postscript window 1 file gNa.ps;
#
