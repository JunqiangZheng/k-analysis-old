source("ziggurat_graph.R")
#result_analysis <- analyze_network("M_PL_098.csv", directory = "data/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = TRUE)
# ziggurat_graph("data/","M_SD_017.csv", plotsdir="named/", coremax_triangle_height_factor = 1.2,
#                displace_y_a=c(0,0,0.4,0.8), print_to_file = TRUE, kcore_species_name_display = seq(2,5), 
#                factor_hop_x = 2.2, kcore_species_name_break = c(5), 
#                height_box_y_expand =2, shorten_species_name = 4)
# ziggurat_graph("data/","M_SD_015.csv", plotsdir="named/", coremax_triangle_height_factor = 1.2,
#                 factor_hop_x = 1.5, print_to_file = TRUE, kcore_species_name_break = c(), 
#                kcore_species_name_display = c(), shorten_species_name =4 )

# ziggurat_graph("data/","M_PL_012.csv", plotsdir="named/", aspect_ratio = 1.4, lsize_legend = 5.5,
#                coremax_triangle_height_factor = 1.6,height_box_y_expand =2, factor_hop_x = 2,
#                lsize_kcoremax = 4, lsize_zig = 3, print_to_file = TRUE,
#                displace_y_a=c(0,0.4,0,0),displace_y_b=c(0,0,0.5,0),color_link = "azure4",
#                kcore_species_name_display = seq(2,4), kcore_species_name_break = c(4), shorten_species_name = 4 )

ziggurat_graph("data/","M_PL_098.csv", plotsdir="named/", coremax_triangle_width_factor = 2,
               coremax_triangle_height_factor = 1.6,displace_legend = c(0,0.2), 
               kcore1tail_disttocore = c(7,1.24),rescale_plot_area=c(1,0.8),
               print_to_file = TRUE, height_box_y_expand =5,
               kcore_species_name_display = seq(2,11),paintlinks=FALSE,
               displace_y_a=c(0,0.2,0.3,0.4,0.4,0.6,0.6,0.6,0.75,0.9,0.9),
               displace_y_b=c(0,-0.6,0.2,0.3,0.3,0.4,0.55,0.55,0.65,0.7),
               shorten_species_name = 4,aspect_ratio = 10,
               factor_hop_x = 8)

#kcore_species_name_break = c(), 
#                kcore_species_name_display = c(), shorten_species_name =4 )