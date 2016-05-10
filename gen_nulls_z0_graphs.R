library(kcorebip)
source("matrix_graph.R")


# ziggurat_graph("nullnetworks/","null_avg_M_PL_012.csv",plotsdir="nullnetworks/",print_to_file = TRUE, 
#                color_link = "darkolivegreen", alpha_link = 0.4, size_link = 0.4, 
#                show_title = TRUE, height_box_y_expand = 4,weirdskcore2_horizontal_dist_rootleaf_expand = 0.5,
#                color_guild_a = c("palegreen3","palegreen4"), displace_outside_component = c(1,0.7),
#                color_guild_b = c("hotpink3","hotpink4"),kcore1tail_disttocore = c(3.5,1.4),
#                lsize_zig = 4.5, lsize_kcore1 = 4, lsize_kcoremax = 5,kcore2tail_vertical_separation = 6, 
#                lsize_core_box = 4, factor_hop_x=1.8,root_weird_expand = c(1.6,1),
#                displace_y_b=c(0,-1.8,-0.6,0,0,0,0),weirds_boxes_separation_count=3.5,
#                coremax_triangle_width_factor = 1.2,coremax_triangle_height_factor = 1.5,
#                displace_legend = c(0.4,0.2),paint_outsiders = FALSE,
#                outsiders_separation_expand = 2)

polar_graph("M_PL_012_lnk_65.csv","nullnetworks/",plotsdir="nullnetworks/",print_to_file=TRUE, lsize_title = 24, 
            lsize_axis = 18, lsize_legend = 18, lsize_axis_title = 18, lsize_legend_title = 20,
            printable_labels = 0)

polar_graph("null_avg_M_PL_012.csv","nullnetworks/",plotsdir="nullnetworks/",print_to_file=TRUE, lsize_title = 24, 
            lsize_axis = 18, lsize_legend = 18, lsize_axis_title = 18, lsize_legend_title = 20,
            printable_labels = 0)

polar_graph("M_PL_012.csv","data/",plotsdir="nullnetworks/",print_to_file=TRUE, lsize_title = 24,
            lsize_axis = 18, lsize_legend = 18, lsize_axis_title = 18, lsize_legend_title = 20,
            printable_labels = 0)

# ziggurat_graph("nullnetworks/","M_PL_012_lnk_65.csv",plotsdir="nullnetworks/",print_to_file = TRUE, 
#                color_link = "darkolivegreen", alpha_link = 0.4, size_link = 0.4, 
#                show_title = TRUE, height_box_y_expand = 4,weirdskcore2_horizontal_dist_rootleaf_expand = 0.5,
#                color_guild_a = c("palegreen3","palegreen4"), displace_outside_component = c(-1,0.7),
#                color_guild_b = c("hotpink3","hotpink4"),kcore1tail_disttocore = c(3.5,1.4),
#                lsize_zig = 4.5, lsize_kcore1 = 4.5, lsize_kcoremax = 5,kcore2tail_vertical_separation = 6, 
#                lsize_core_box = 4, factor_hop_x=1.8,root_weird_expand = c(0.5,1),
#                displace_y_b=c(0,-1.8,-0.6,0,0,0,0),weirds_boxes_separation_count=3.5,
#                coremax_triangle_width_factor = 1.4,coremax_triangle_height_factor = 2,
#                displace_legend = c(0,0.2),kcore1weirds_leafs_vertical_separation = 2,paint_outsiders = FALSE,
#                outsiders_separation_expand = 2)

# ziggurat_graph("nullnetworks/","null_avg_M_PL_046.csv",plotsdir="nullnetworks/",print_to_file = TRUE, 
#                color_link = "slategray3", show_title = TRUE, height_box_y_expand = 1.5,
#                color_guild_a = c("palegreen3","palegreen4"), 
#                color_guild_b = c("hotpink3","hotpink4"),
#                displace_y_b=c(0,0,0,0.5,0.5,0),aspect_ratio=1,
#                lsize_zig = 4.5, lsize_kcore1 = 4, lsize_kcoremax = 5,
#                lsize_core_box = 4,
#                coremax_triangle_width_factor = 1.2,
#                alpha_link = 0.7, size_link = 0.4 )

# polar_graph("null_avg_M_PL_046.csv","nullnetworks/",plotsdir="nullnetworks/",print_to_file=TRUE, lsize_title = 24, 
#             lsize_axis = 18, lsize_legend = 18, lsize_axis_title = 18, lsize_legend_title = 20)

# ziggurat_graph("nullnetworks/","M_PL_046_lnk_136.csv",plotsdir="nullnetworks/",print_to_file = TRUE, 
#                color_link = "slategray3", show_title = TRUE, height_box_y_expand = 1.5,
#                color_guild_a = c("palegreen3","palegreen4"), 
#                color_guild_b = c("hotpink3","hotpink4"),kcore1tail_disttocore = c(2,1.25),
#                displace_y_b=c(0,0,0,0.3,0.7,1),aspect_ratio=1,
#                lsize_zig = 4.5, lsize_kcore1 = 4, lsize_kcoremax = 5,
#                lsize_core_box = 4,
#                coremax_triangle_width_factor = 1.2,
#                alpha_link = 0.5, size_link = 0.4 )


# polar_graph("M_PL_046_lnk_136.csv","nullnetworks/",plotsdir="nullnetworks/",print_to_file=TRUE, lsize_title = 24, 
#             lsize_axis = 18, lsize_legend = 18, lsize_axis_title = 18, lsize_legend_title = 20)