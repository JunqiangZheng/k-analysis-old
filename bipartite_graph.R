source("network-kanalysis.R")

plot_bipartite <- function(bg, aspect_ratio = 9/35, vframecolor = "grey70", vlabelcex = 4,
                         vsize = 4, vcolor = c("lightblue","pink2"), 
                         framedisp = FALSE,  color_link = "grey50")
{
  plot.igraph(bg, layout=layout.bipartite,asp=aspect_ratio,vertex.frame.color=vframecolor,
              vertex.label.cex=vlabelcex,
              vertex.size=vsize, edge.color= color_link, frame = framedisp,
              vertex.color=vcolor[V(bg)$type+1])
}
#result_analysis <- analyze_network("M_PL_030.csv", directory = "data/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = TRUE)