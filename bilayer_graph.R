source("network-kanalysis.R")

plot_bilayer <- function(bg, aspect_ratio = 9/35, vframecolor = "grey70", vlabelcex = 4,
                         vsize = 4, vcolor = c("lightblue","pink2"), 
                         edgecolor = "grey50", framedisp = FALSE)
{
  plot.igraph(bg, layout=layout.bipartite,asp=aspect_ratio,vertex.frame.color=vframecolor,
              vertex.label.cex=vlabelcex,
              vertex.size=vsize, edge.color= edgecolor, frame = framedisp,
              vertex.color=vcolor[V(bg)$type+1])
}
#result_analysis <- analyze_network("M_PL_030.csv", directory = "data/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = TRUE)