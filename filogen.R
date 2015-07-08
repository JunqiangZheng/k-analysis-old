source("ziggurat_graph.R")
#result_analysis <- analyze_network("M_PL_030.csv", directory = "data/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = TRUE)
# ziggurat_graph("data/","M_SD_017.csv", plotsdir="named/", coremax_triangle_height_factor = 1.2,
#                displace_y_a=c(0,0,0.4,0.8), print_to_file = TRUE, kcore_species_name_display = seq(2,5), 
#                factor_hop_x = 2.2, kcore_species_name_break = c(5), 
#                color_link = "darkolivegreen", alpha_link = 0.4, size_link = 0.3,
#                height_box_y_expand =2, shorten_species_name = 4, spline_points = 1000)
# ziggurat_graph("data/","M_SD_015.csv", plotsdir="named/", coremax_triangle_height_factor = 1,
#                 factor_hop_x = 1.5, print_to_file = TRUE, kcore_species_name_break = c(), 
#                color_link = "slategray3", alpha_link = 0.1, size_link = 0.25, 
#                kcore_species_name_display = c(), shorten_species_name =4 )

# ziggurat_graph("data/","M_PL_012.csv", plotsdir="named/", aspect_ratio = 1.4, lsize_legend = 5.5,
#                coremax_triangle_height_factor = 1.6,height_box_y_expand =2, factor_hop_x = 2,
#                lsize_kcoremax = 4, lsize_zig = 3, print_to_file = TRUE,   paintlinks = FALSE,
#                displace_y_a=c(0,0.4,0,0),displace_y_b=c(0,0,0.5,0),color_link = "azure4",
#                kcore_species_name_display = seq(2,4), kcore_species_name_break = c(4), shorten_species_name = 4 )
#

init_time <- proc.time()
# ziggurat_graph("data/","M_SD_004.csv", plotsdir="named/",
#                horiz_kcoremax_tails_expand = 4,height_box_y_expand = 3, factor_hop_x = 2,
#                lsize_legend = 5.5,lsize_kcoremax = 5,lsize_zig = 4.5,lsize_kcore1 = 4.5,lsize_core_box = 4.5,
#                displace_y_a=c(0,0.4,0,0),displace_y_b=c(0,0.1,0,0),
#                kcore1tail_disttocore = c(1.25,0.8),
#                fattailjumpvert = c(2,2),    paintlinks = TRUE,
#                displace_outside_component = c(-0.7,0.8), outsiders_legend_expand = -0.2,
#                color_link = "bisque4", alpha_link = 0.4, size_link = 0.4,
#                spline_points=1000,
#                show_title = FALSE, print_to_file = FALSE)

ziggurat_graph("data/","M_SD_004.csv", plotsdir="poko/",displaylabelszig = TRUE,
               horiz_kcoremax_tails_expand = 4,height_box_y_expand = 3, factor_hop_x = 2,
               lsize_legend = 5.5,lsize_kcoremax = 5,lsize_zig = 4.5,lsize_kcore1 = 4.5,lsize_core_box = 4.5,
               displace_y_a=c(0,0.4,0,0),displace_y_b=c(0,0.1,0,0),
               kcore1tail_disttocore = c(1.25,0.8), 
               fattailjumpvert = c(2,2),    paintlinks = TRUE, landscape_plot = TRUE,
               displace_outside_component = c(-0.7,0.8),
               color_link = "bisque4", alpha_link = 0.4, size_link = 0.4,
               spline_points=100,  use_spline = TRUE,
               show_title = FALSE, print_to_file = FALSE)

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
#                print_to_file = TRUE)

# ziggurat_graph("data/","M_PL_098.csv", plotsdir="named/", coremax_triangle_width_factor = 1.2,
#                coremax_triangle_height_factor = 3,displace_legend = c(0.8,2.5),
#                kcore1tail_disttocore = c(10,1.24),rescale_plot_area=c(1,0.35),
#                print_to_file = TRUE, height_box_y_expand =40,lsize_zig = 2.5,lsize_kcoremax = 2.5,
#                kcore_species_name_display = seq(2,11),
#                color_link = "slategray3", alpha_link = 0.1, size_link = 0.25,

# ziggurat_graph("data/","M_PL_001.csv", plotsdir="named/", height_box_y_expand =3,innertail_vertical_separation = 1.5,
#                displace_y_a=c(0,0.3,0.6), factor_hop_x=1.5, coremax_triangle_width_factor = 1.1,
#                kcore1tail_disttocore = c(1.6,1.2), displace_outside_component = c(-0.7,1.2),
#                color_link = "Lavender", alpha_link = 0.7, size_link = 0.4
#                )



