source("ziggurat_graph.R")
result_analysis <- analyze_network("M_PL_098.csv", directory = "data/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = TRUE)
# ziggurat_graph("data/","M_SD_017.csv", plotsdir="named/", coremax_triangle_height_factor = 1.2,
#                displace_y_a=c(0,0,0.4,0.8), print_to_file = TRUE, kcore_species_name_display = seq(2,5), 
#                factor_hop_x = 2.2, kcore_species_name_break = c(5), 
#                height_box_y_expand =2, shorten_species_name = 4)
ziggurat_graph("data/","M_SD_015.csv", plotsdir="named/", coremax_triangle_height_factor = 1.2,
                factor_hop_x = 1.5, print_to_file = TRUE, kcore_species_name_break = c(), color_link = "slategray3", alpha_link = 0.1, size_link = 0.25, 
               kcore_species_name_display = c(), shorten_species_name =4 )

# ziggurat_graph("data/","M_PL_012.csv", plotsdir="named/", aspect_ratio = 1.4, lsize_legend = 5.5,
#                coremax_triangle_height_factor = 1.6,height_box_y_expand =2, factor_hop_x = 2,
#                lsize_kcoremax = 4, lsize_zig = 3, print_to_file = TRUE,   paintlinks = FALSE,
#                displace_y_a=c(0,0.4,0,0),displace_y_b=c(0,0,0.5,0),color_link = "azure4",
#                kcore_species_name_display = seq(2,4), kcore_species_name_break = c(4), shorten_species_name = 4 )


ziggurat_graph("data/","M_PL_098.csv", plotsdir="named/", coremax_triangle_width_factor = 1.2,
               coremax_triangle_height_factor = 3,displace_legend = c(0.8,2.5),
               kcore1tail_disttocore = c(10,1.24),rescale_plot_area=c(1,0.35),
               print_to_file = TRUE, height_box_y_expand =40,lsize_zig = 2.5,lsize_kcoremax = 2.5,
               kcore_species_name_display = seq(2,11),
               color_link = "slategray3", alpha_link = 0.1, size_link = 0.25,
               displace_y_a=c(0,0.4,0.5,1,1,1.7,1.8,1.9,2.15,2.5),
               displace_y_b=c(0,-1,0.6,0.7,0.7,0.8,0.95,0.95,1,1.25),innertail_vertical_separation = 3,
               shorten_species_name = 4,aspect_ratio = 7,
               factor_hop_x =68)

