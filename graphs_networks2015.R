source("ziggurat_graph.R")
source("matrix_graph.R")
source("bipartite_graph.R")

# result_analysis <- analyze_network("M_SD_002.csv", directory = "data/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = FALSE)
# bp <- get_bipartite(result_analysis$graph, plot_graphs = FALSE)
# plot_bipartite(bp, aspect_ratio = 9/45,vlabelcex=1.5,vsize = 5, vframecolor = "grey40", color_link = "black")


matrix_graph("M_PL_002a.csv",plotsdir = "networks2015", lsize_axis = 15, plot_klines = FALSE, scale_color_dots =FALSE, printfile=TRUE, aspect_ratio = 1.25)

# ziggurat_graph("data/",plotsdir = "networks2015","M_SD_007.csv", rescale_plot_area=c(1,0.33), 
#                lsize_kcoremax = 5.5,lsize_zig = 4.5,lsize_kcore1 = 4.5,lsize_core_box = 6,labels_size=6,
#                lsize_legend = 6,displace_legend = c(-0.35,-0.3),
#                coremax_triangle_width_factor = 1.7,coremax_triangle_height_factor = 1.8,
#                height_box_y_expand =4,aspect_ratio =0.75,
#                kcore1tail_disttocore = c(3,1),fattailjumpvert = c(2,1),
#                displace_y_a=c(0,-0.6),
#                print_to_file = TRUE)

# ziggurat_graph("datanetworks2015/",plotsdir = "networks2015","M_SD_007_rnd_6.csv",
#                height_box_y_expand = 6,aspect_ratio=0.85,
#                color_guild_a = c("palegreen3","palegreen4"), 
#                color_guild_b = c("hotpink3","hotpink4"),
#                displace_y_a=c(0,0.2,0.6),
#                lsize_kcoremax = 5.5,lsize_zig = 4.5,lsize_kcore1 = 4.5,lsize_core_box = 6,labels_size=6,
#                lsize_legend = 6,displace_legend = c(-0.25,-0.3),factor_hop_x = 2,
#                print_to_file = TRUE, paint_outsiders = FALSE)


# ziggurat_graph("datanetworks2015/","M_PL_012_rnd_17.csv", plotsdir ="networks2015/",aspect_ratio = 1, 
#                height_box_y_expand = 2, factor_hop_x=1.5,
#                displace_legend = c(-0.25,0.15),coremax_triangle_height_factor = 1.8,
#                coremax_triangle_width_factor = 1.2,
#                lsize_legend = 5.5, innertail_vertical_separation = 2,
#                lsize_core_box = 5,lsize_kcoremax = 4.5,
#                color_guild_a = c("palegreen3","palegreen4"), 
#                color_guild_b = c("hotpink3","hotpink4"),
#                paint_outsiders = FALSE,
#                lsize_zig = 4.5, lsize_kcore1 = 4, horiz_kcoremax_tails_expand = 4,
#                displace_y_a=c(0,0.25,0,0),displace_y_b=c(0,-0.9,0,0),
#                print_to_file = TRUE)

# ziggurat_graph("datanetworks2015/","M_PL_012_rnd_7.csv", plotsdir ="networks2015/",aspect_ratio = 1, 
#                height_box_y_expand = 3, factor_hop_x=1.5,
#                displace_legend = c(-0.25,0),coremax_triangle_height_factor = 1,
#                coremax_triangle_width_factor = 1.2,
#                lsize_legend = 5.5, innertail_vertical_separation = 2,
#                lsize_core_box = 5,lsize_kcoremax = 4.5,
#                color_guild_a = c("palegreen3","palegreen4"), 
#                color_guild_b = c("hotpink3","hotpink4"),
#                paint_outsiders = FALSE,
#                lsize_zig = 4.5, lsize_kcore1 = 4, horiz_kcoremax_tails_expand = 4,
#                displace_y_b=c(0,0,0.2,0),
#                print_to_file = TRUE)

# ziggurat_graph("datanetworks2015/","M_PL_012_rnd_32.csv", plotsdir ="networks2015/",aspect_ratio = 1.2, 
#                height_box_y_expand = 3, factor_hop_x=1.5,
#                displace_legend = c(-0.25,0.25),coremax_triangle_height_factor = 1.2,
#                coremax_triangle_width_factor = 1.2,
#                lsize_legend = 5.5, innertail_vertical_separation = 2,
#                lsize_core_box = 5,lsize_kcoremax = 4.5,
#                color_guild_a = c("palegreen3","palegreen4"), 
#                color_guild_b = c("hotpink3","hotpink4"),
#                paint_outsiders = FALSE,
#                kcore2tail_vertical_separation = 1.8,
#                lsize_zig = 4.5, lsize_kcore1 = 4, horiz_kcoremax_tails_expand = 4,
#                displace_y_a=c(0,0.25,0,0),displace_y_b=c(0,-1.2,0,0),
#                print_to_file = TRUE)

# ziggurat_graph("data/","M_PL_047.csv",displace_legend = c(-0.2,0.1),
#                displace_y_b=c(0,0,0,0.2,0.3),
#                paintlinks=TRUE, kcore1tail_disttocore = c(2,0.95),rescale_plot_area=c(1,1.2),
#                height_box_y_expand =7 , innertail_vertical_separation = 2.5,
#                aspect_ratio  = .4,
#                color_link = "slategray3", alpha_link = 0.7, size_link = 0.3,
#                plotsdir = "networks2015/",
#                factor_hop_x=1.5,print_to_file = TRUE)

# 
#   ziggurat_graph("datanetworks2015/","pl017-minus6plants.csv", plotsdir = "networks2015/",height_box_y_expand =2.5,displace_y_b=c(0,0,0.5,0.5,0.5),
#                  lsize_legend = 7.5,lsize_kcoremax = 4,lsize_zig = 3.5,lsize_kcore1 = 3.5,lsize_core_box = 4, 
#                  color_link = "slategray3", alpha_link = 0.7,
#                  labels_color = c("navyblue","red4"),coremax_triangle_width_factor = 1.3, paint_outsiders = FALSE,
#                  show_title = FALSE, displace_legend = c(-0.25,0.2), aspect_ratio = 0.75, print_to_file = TRUE)
  
# ziggurat_graph("data/","M_PL_010.csv", plotsdir = "networks2015/",
#                   height_box_y_expand =1.5,coremax_triangle_width_factor = 1.25, 
#                kcore1tail_disttocore = c(1.3,1.1), 
#                show_title = FALSE, displace_legend = c(-0.1,0),
#                alpha_link = 0.4, size_link = 0.4,
#                displace_y_b=c(0,-0.15,-0.15,0,0.1,0,0,0),print_to_file = TRUE, aspect_ratio = 1.2)


#   ziggurat_graph("data/","M_PL_046.csv", plotsdir = "networks2015/",height_box_y_expand =2.5,displace_y_b=c(0,0,0.5,0.5,0.5),
#                  lsize_legend = 7.5,lsize_kcoremax = 4,lsize_zig = 3.5,lsize_kcore1 = 3.5,lsize_core_box = 4, 
#                  color_link = "slategray3", alpha_link = 0.7,
#                  labels_color = c("navyblue","red4"),coremax_triangle_width_factor = 1.3, paint_outsiders = FALSE,
#                  show_title = FALSE, displace_legend = c(-0.25,0.2), aspect_ratio = 0.75, print_to_file = TRUE)
  
#   ziggurat_graph("data/","M_PL_046.csv",  plotsdir = "networks2015/",paintlinks = TRUE,displace_legend = c(-0.3,0.4), 
#                  color_link = "slategray3", alpha_link = 0.7,
#                                  displace_outside_component = c(-0.5,0.5), rescale_plot_area=c(0.8,0.65), 
#                                  aspect_ratio=1.15,print_to_file = TRUE)

# ziggurat_graph("data/","M_PL_024.csv", plotsdir ="networks2015/",
#                lsize_legend =5, lsize_core_box = 5,lsize_kcoremax = 5,lsize_zig = 5, print_to_file = TRUE,
#                color_link = "darkolivegreen", alpha_link = 0.4, size_link = 0.4,
#                lsize_kcore1 =4.5)

# ziggurat_graph("data/","M_SD_004.csv", plotsdir ="networks2015/",lsize_legend = 5.5, height_box_y_expand =2,
#                                                lsize_core_box = 5,lsize_kcoremax = 4.5,  lsize_zig = 4,
#                lsize_kcore1 = 4,aspect_ratio=0.8,displace_y_a=c(0,0.3,0,0,0),
#                displace_outside_component = c(-0.6,0.7),
#               color_link = "slategray3", alpha_link = 0.7,print_to_file = TRUE)  

# 
# ziggurat_graph("datanetworks2015/","M_SD_004_pol3_pol4.csv", plotsdir ="networks2015/",
#               lsize_legend = 7, height_box_y_expand =2,
#               coremax_triangle_height_factor = 1.6,root_weird_expand = c(0.6,1.6),
#               lsize_core_box = 7,lsize_kcoremax = 7,  lsize_zig = 6,
#               lsize_kcore1 = 7, fattailjumpvert = c(2,2),aspect_ratio = 0.5,
#               paint_outsiders = FALSE,displace_legend = c(-0.25,0),
#               color_link = "slategray3", alpha_link = 0.7,print_to_file = TRUE)


# ziggurat_graph("datanetworks2015/","M_SD_004_minus_k4.csv", plotsdir ="networks2015/",
#               lsize_legend = 7, height_box_y_expand =2,
#               coremax_triangle_height_factor = 1.6,root_weird_expand = c(0.6,1.6),
#               lsize_core_box = 7,lsize_kcoremax = 7,  lsize_zig = 6,
#               lsize_kcore1 = 7, fattailjumpvert = c(2,2),aspect_ratio = 0.5,
#               paint_outsiders = FALSE,displace_legend = c(-0.25,0),
#               color_link = "slategray3", alpha_link = 0.7,print_to_file = FALSE)

# 
# ziggurat_graph("data/","M_SD_016.csv",plotsdir ="networks2015/",height_box_y_expand =2.5,
#                color_link = "slategrey", alpha_link = 0.3, size_link = 0.4, 
#                coremax_triangle_width_factor = 1.6,lsize_kcore1 = 4,
#                lsize_core_box = 5,lsize_kcoremax = 4.5,  lsize_zig = 4,
#                displace_y_b=c(0,0,0,0,0,0.05,0.1,0.2,0.4,0.6),
#                displace_y_a=c(0,0,0,0,0,0,0,0,0.2,0.4),
#                kcore1tail_disttocore = c(1.5,1.3),
#                factor_hop_x=1.2, lsize_legend = 5.5,
#                displace_legend = c(0,0.3),
#                print_to_file = TRUE)
# 
# ziggurat_graph("data/","M_PL_031.csv", plotsdir ="networks2015/",outsiders_legend_expand = -0.2,
#                lsize_core_box = 5,lsize_kcoremax = 4.5,lsize_kcore1 = 4, lsize_zig = 4,
#                color_link = "slategray3", alpha_link = 0.4,root_weird_expand = c(0.45,1),
#                show_title = FALSE,
#                displace_y_a=c(0,0.3,0,0),displace_y_b=c(0,0.2,0,0),
#                aspect_ratio = 0.33, height_box_y_expand =3, 
#                displace_legend = c(0,0.1),print_to_file = TRUE)

# ziggurat_graph("data/","M_PL_012.csv", plotsdir ="networks2015/",aspect_ratio = 1, height_box_y_expand = 2, factor_hop_x=1.5,
#                                lsize_legend = 5.5, innertail_vertical_separation = 2,
#                                lsize_core_box = 5,lsize_kcoremax = 4.5, 
#                color_link = "darkolivegreen", alpha_link = 0.4, size_link = 0.4,
#                               lsize_zig = 4.5, lsize_kcore1 = 4, horiz_kcoremax_tails_expand = 4,displace_y_a=c(0,0.5,0,0),
#                print_to_file = TRUE)
  
# ziggurat_graph("data/","M_SD_017.csv", plotsdir="named/", coremax_triangle_height_factor = 1.2,
#                displace_y_a=c(0,0,0.4,0.8), print_to_file = TRUE, kcore_species_name_display = seq(2,5), 
#                factor_hop_x = 2.2, kcore_species_name_break = c(5), 
#                color_link = "darkolivegreen", alpha_link = 0.4, size_link = 0.3,
#                height_box_y_expand =2, shorten_species_name = 4, spline_points = 1000)
# ziggurat_graph("data/","M_SD_015.csv", plotsdir="networks2015/", coremax_triangle_height_factor = 1,
#                 factor_hop_x = 1.5, kcore_species_name_break = c(), 
#                height_box_y_expand =2,
#                color_link = "darkolivegreen", alpha_link = 0.4, size_link = 0.4,
#                lsize_legend = 6, 
#                lsize_core_box = 5,lsize_kcoremax = 5.5, 
#                lsize_zig = 5, lsize_kcore1 = 5, horiz_kcoremax_tails_expand = 4,displace_y_b=c(0,0,0.5,0),
#                print_to_file = TRUE)
               
# ziggurat_graph("data/","M_PL_012.csv", plotsdir="named/", aspect_ratio = 1.4, lsize_legend = 5.5,
#                coremax_triangle_height_factor = 1.6,height_box_y_expand =2, factor_hop_x = 2,
#                lsize_kcoremax = 4, lsize_zig = 3, print_to_file = TRUE,   paintlinks = FALSE,
#                displace_y_a=c(0,0.4,0,0),displace_y_b=c(0,0,0.5,0),color_link = "azure4",
#                kcore_species_name_display = seq(2,4), kcore_species_name_break = c(4), shorten_species_name = 4 )


init_time <- proc.time()

# ziggurat_graph("data/","M_SD_004.csv", plotsdir="named/", alpha_level = 0.4,
#                horiz_kcoremax_tails_expand = 4,height_box_y_expand = 3, factor_hop_x = 2,
#                lsize_legend = 7.5,lsize_kcoremax = 6,lsize_zig = 5.5,lsize_kcore1 = 5.5,lsize_core_box = 6,
#                labels_color = c("navyblue","red4"),
#                displace_y_a=c(0,0.4,0,0),displace_y_b=c(0,0.1,0,0),
#                kcore1tail_disttocore = c(1.25,0.8),
#                fattailjumpvert = c(2,2),    paintlinks = TRUE,
#                displace_outside_component = c(-0.7,0.8), outsiders_legend_expand = -0.2,
#                color_link = "darkolivegreen", alpha_link = 0.4, size_link = 0.4,
#                spline_points=1000,
#                show_title = FALSE, print_to_file = TRUE)

end_time <- proc.time()
print("Tiempo total")
print(end_time - init_time)


# ziggurat_graph("data/","M_SD_004.csv", plotsdir="named/", height_box_y_expand =8,factor_hop_x = 4.5,
#                rescale_plot_area=c(1,0.3),coremax_triangle_height_factor = 1.6,
#                displace_y_a=c(0,1,0,0),displace_y_b=c(0,0,0.2,0),displace_legend = c(0.15,1.75),show_title = FALSE,
#                lsize_legend = 5.5,lsize_kcoremax = 5,lsize_zig = 4.5,lsize_kcore1 = 4.5,
#                lsize_core_box = 4.5,outsiders_separation_expand = 2, outsiders_legend_expand = -0.2,
#                displace_outside_component = c(-0.7,1),kcore1tail_disttocore = c(1.4,1),
#                color_link = "darkolivegreen", alpha_link = 0.4, size_link = 0.3,
#                fattailjumpvert = c(1.5,2), horiz_kcoremax_tails_expand = 10,
#                print_to_file = FALSE)

# ziggurat_graph("data/","CAB_M_PL_098.csv", plotsdir="named/", coremax_triangle_width_factor = 1.2,
#                coremax_triangle_height_factor = 3,displace_legend = c(0.8,2.5),
#                kcore1tail_disttocore = c(10,1.24),rescale_plot_area=c(1,0.35),
#                print_to_file = FALSE, height_box_y_expand =40,lsize_zig = 2.5,lsize_kcoremax = 2.5,
#                kcore_species_name_display = seq(2,11),
#                color_link = "slategray3", alpha_link = 0.1, size_link = 0.25,
#                factor_hop_x = 20, paintlinks = FALSE)

# ziggurat_graph("data/","M_PL_001.csv", plotsdir="named/", height_box_y_expand =3,innertail_vertical_separation = 1.5,
#                displace_y_a=c(0,0.3,0.6), factor_hop_x=1.5, coremax_triangle_width_factor = 1.1,
#                kcore1tail_disttocore = c(1.6,1.2), displace_outside_component = c(-0.7,1.2),
#                color_link = "Lavender", alpha_link = 0.7, size_link = 0.4
#                )


