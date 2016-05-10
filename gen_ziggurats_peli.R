# Code to generate ziggurat plots of destruction removing only animals for the Science movie

rm(list=ls())
library(kcorebip)

# 
# ziggurat_graph("data/","M_PL_007.csv", height_box_y_expand = 0.75, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_1 by MusRank.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_2 by MusRank.csv", height_box_y_expand = 1.25, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_3 by MusRank.csv", height_box_y_expand = 1.25, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_4 by MusRank.csv", height_box_y_expand = 1.25, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_5 by MusRank.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_6 by MusRank.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_7 by MusRank.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_8 by MusRank.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_9 by MusRank.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_10 by MusRank.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)



# ziggurat_graph("datapeli/","M_PL_007 minus_1 by Kdegree.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_2 by Kdegree.csv", height_box_y_expand = 1.25, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_3 by Kdegree.csv", height_box_y_expand = 1.25, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_4 by Kdegree.csv", height_box_y_expand = 1.25, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_5 by Kdegree.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.2),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)


# ziggurat_graph("datapeli/","M_PL_007 minus_6 by Kdegree.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1, 
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.4),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_7 by Kdegree.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.4),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_8 by Kdegree.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.4),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_9 by Kdegree.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.4),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)
# 
# ziggurat_graph("datapeli/","M_PL_007 minus_10 by Kdegree.csv", height_box_y_expand = 1, 
#                lsize_legend = 7, lsize_core_box = 6,corebox_border_size=1,
#                plotsdir = "datapeli/peli",color_link = "slategray3", alpha_link = 0.7,root_weird_expand = c(1,1.35),
#                lsize_kcoremax = 6,lsize_zig = 5,lsize_kcore1 = 5, paint_outsiders = FALSE,kcore1tail_disttocore = c(1.3,1),
#                displace_legend = c(-0.2,0.4),displace_outside_component = c(-0.5,0.6),print_to_file = TRUE)


count_species <- function(fich,p)
{
  speciesGC <- subset(p,p$kradius!=Inf)
  tot <- length(speciesGC)
  npol <- length(grep("Pollinator",names(speciesGC)))
  print(paste(fich,"Giant Component. Plants ", tot-npol,"pols",npol, "tot", tot))
}

fich <- "M_PL_007.csv"
result_analysis <- analyze_network(fich,directory = "data/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = FALSE)
count_species(fich,V(result_analysis$graph))

for (i in seq(1,10)) {
fich <- paste0("M_PL_007 minus_",i," by Kdegree.csv")
result_analysis <- analyze_network(fich,directory = "datapeli/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = FALSE)
count_species(fich,V(result_analysis$graph))
}


for (i in seq(1,10)) {
  fich <- paste0("M_PL_007 minus_",i," by MusRank.csv")
  result_analysis <- analyze_network(fich,directory = "datapeli/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = FALSE)
  count_species(fich,V(result_analysis$graph))
}
