source("ziggurat_graph.R")
source("matrix_graph.R")
source("polar_graph.R")
# result_analysis <- analyze_network("epifitecuador.csv", directory = "datacom/", 
#                                    guild_a = "Plant", guild_b = "Pollinator", plot_graphs = TRUE)
ziggurat_graph("datacom/","epifitecuador.csv", print_to_file = FALSE,  height_box_y_expand =6,
               kcore_species_name_display = seq(2,7), paintlinks=TRUE,
               shorten_species_name =4,coremax_triangle_height_factor = 2,
               label_strguilda = "Orchid", label_strguildb = "Host",  factor_hop_x=2.5,
               lsize_zig = 2.5,lsize_kcoremax = 2.5, rescale_plot_area=c(0.6,1),
               color_link = "slategray3", alpha_link = 0.15, size_link = 0.35,
               color_guild_a=c("darkolivegreen","darkolivegreen3"), color_guild_b=c("coral2","coral4"),
               displace_y_a=c(0,0,0.4,0.5,0.6,0.7), displace_y_b=c(0,-0.6,-0.4,-0.1,0.65,0.4))
# 
# ziggurat_graph("datacom/","epifitecuador.csv", print_to_file = TRUE,  height_box_y_expand =6, paintlinks=TRUE,
#                coremax_triangle_height_factor = 2,
#                label_strguilda = "Orchid", label_strguildb = "Host",  factor_hop_x=2.5,
#                lsize_zig = 2.5,lsize_kcoremax = 2.5, rescale_plot_area=c(0.6,1),
#                color_link = "slategray3", alpha_link = 0.15, size_link = 0.35,
#                color_guild_a=c("darkolivegreen","darkolivegreen3"), color_guild_b=c("coral2","coral4"),
#                displace_y_a=c(0,0,0.4,0.5,0.6,0.7), displace_y_b=c(0,-0.6,-0.4,-0.1,0.65,0.4))
# 
# matrix_graph("epifitecuador",dirdata = "datacom/", label_strguilda = "Orchid", label_strguildb = "Host",
#              color_guild_a=c("darkolivegreen","darkolivegreen3"), color_guild_b=c("coral2","coral4"))
# 
# polar_graph("epifitecuador.csv","datacom/",print_to_file=TRUE, glabels=c("Orchid","Host"), gshortened = c("or","ho"))