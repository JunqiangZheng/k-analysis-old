rm(list=ls())

source("ziggurat_graph.R")
init_time <- proc.time()

#ziggurat_graph("data/","M_PL_002.csv", print_to_file = TRUE)



end_time <- proc.time()
print(end_time - init_time)

#ziggurat_graph("data/","M_PL_001.csv", height_box_y_expand =3, innertail_vertical_separation = 1.5, aspect_ratio=0.8,paintlinks=TRUE,coremax_triangle_width_factor = 1.5,kcore1tail_disttocore = c(1.6,1.2), displace_outside_component = c(-0.7,1.2), print_to_file = TRUE)
#ziggurat_graph("data/","M_PL_002.csv", print_to_file = TRUE)
#ziggurat_graph("data/","M_PL_003.csv", aspect_ratio = 2, displace_legend = c(-0.5,-0.2), print_to_file = TRUE)
#ziggurat_graph("data/","M_PL_004.csv", height_box_y_expand =2,displace_outside_component = c(-0.5,0.4),displace_y_b=c(0,-0.7),print_to_file = TRUE)
# ziggurat_graph("data/","M_PL_005.csv", paintlinks = TRUE,print_to_file = TRUE,kcore1tail_disttocore = c(2.3,0.9),innertail_vertical_separation =2,kcore2tail_vertical_separation = 2, 
#                height_box_y_expand =5,factor_hop_x=2.8,coremax_triangle_height_factor = 1.5,coremax_triangle_width_factor = 1.2,outsiders_separation_expand = 2,weirds_boxes_separation_count = 3,aspect_ratio=1.5,
#                displace_y_a=c(0,0,0,0.18,0.2,0.3,0.5),displace_y_b=c(-0.5,0,0.45,0.62,0.67,0.68,0.7), fattailjumpvert = c(1,0.5), lsize_zig = 2.5,
#                displace_outside_component = c(-0.5,0.6),weirds_vertical_dist_rootleaf_expand = 0.4,root_weird_expand = c(1.2,1))
#ziggurat_graph("data/","M_PL_006.csv",height_box_y_expand = 1.5,print_to_file = TRUE)
#ziggurat_graph("data/","M_PL_007.csv", height_box_y_expand = 0.75,displace_legend = c(0,0.1),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
#ziggurat_graph("data/","M_PL_008.csv", print_to_file = TRUE)
#ziggurat_graph("data/","M_PL_009.csv", height_box_y_expand = 1.2, print_to_file = TRUE)
#ziggurat_graph("data/","M_PL_010.csv", height_box_y_expand =1.5, displace_y_b=c(0,0,-0.15,0,0,0,0,0),print_to_file = TRUE, aspect_ratio = 1.2)
#ziggurat_graph("data/","M_PL_031.csv", aspect_ratio = 0.60, height_box_y_expand =3, print_to_file = TRUE)

