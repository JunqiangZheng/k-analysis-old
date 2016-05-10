library(grid)
library(gridExtra)
library(stargazer)
library(igraph)
library(ggplot2)

library(kcorebip)

red <- c("M_PL_001.csv")
result_analysis <- analyze_network(red, directory = "data/", guild_a = "Plant", 
                                   guild_b = "Pollinator", plot_graphs = FALSE)
ddegree <- igraph::degree(result_analysis$graph,mode = c("out"), loops = TRUE, normalized = FALSE)

occur <- sort(as.numeric(ddegree))
alpha_level = 0.5
p = occur/sum(occur)
dy = rev(cumsum(rev(p)))
dx = occur

krisk <- V(result_analysis$graph)$krisk
occur <- sort(as.numeric(krisk))
p = occur/sum(occur)
ky = rev(cumsum(rev(p)))
kx = occur

alfa <- lm(ddegree ~ krisk)$coefficients[2]

red_name <- strsplit(red,".csv")[[1]][1]

auxdf <- data.frame(dx,dy,kx,ky)
dist_krisk <- ggplot(data = auxdf, aes(x = kx, y = ky)) + geom_point(color = "red") +
  scale_x_log10() + scale_y_log10() + xlab("krisk") + ylab("Cumulative distribution") +
  theme_bw() + ggtitle(red_name) +
  theme(
    axis.title.x = element_text(color="grey30", size=12),
    axis.title.y = element_text(color="grey30", size=12),
    axis.text.x = element_text(face="bold", color="grey30", size=11),
    axis.text.y = element_text(face="bold", color="grey30", size=11)
  )
  
g <- V(result_analysis$graph)

# ppi <- 300
# png("ESTATICA_hist_degrees.png", width=(12*ppi), height=4*ppi, res=ppi)
# grid.arrange(histo_core,histo_dist,histo_deg,ncol=3, nrow=1, widths=c(1/3,1/3,1/3))
# dev.off()