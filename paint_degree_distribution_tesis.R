library(grid)
library(gridExtra)
library(stargazer)
library(igraph)
library(ggplot2)

source("network-kanalysis.R")

red <- c("M_PL_054.csv")
result_analysis <- analyze_network(red, directory = "data/", guild_a = "Plant", 
                                   guild_b = "Pollinator", plot_graphs = FALSE)
ddegree <- igraph::degree(result_analysis$graph,mode = c("out"), loops = TRUE, normalized = FALSE)

occur <- sort(as.numeric(ddegree))
alpha_level = 0.5
p = occur/sum(occur)
dy = rev(cumsum(rev(p)))
dx = occur

kdegree <- V(result_analysis$graph)$kdegree
occur <- sort(as.numeric(kdegree))
p = occur/sum(occur)
ky = rev(cumsum(rev(p)))
kx = occur

alfa <- lm(ddegree ~ kdegree)$coefficients[2]

red_name <- strsplit(red,".csv")[[1]][1]

auxdf <- data.frame(dx,dy,kx,ky)
dist_deg <- ggplot(data = auxdf, aes(x = dx, y = dy)) + geom_point(color = "red") +
  scale_x_log10() + scale_y_log10() + xlab("degree") + ylab("Cumulative distribution") +
  theme_bw() + ggtitle(red_name) +
  theme(
    axis.title.x = element_text(color="grey30", size=12),
    axis.title.y = element_text(color="grey30", size=12),
    axis.text.x = element_text(face="bold", color="grey30", size=11),
    axis.text.y = element_text(face="bold", color="grey30", size=11)
  )
  
#plot(x, y, log="xy", type="l")


#plot(x, y, log="xy", type="l")

dist_kdeg <- ggplot(data = auxdf, aes(x = kx, y = ky)) + geom_point(color = "darkgreen") +
  scale_x_log10() + scale_y_log10() + xlab("k degree") + ylab("Cumulative distribution") +
  theme_bw() + ggtitle(red_name) +
  theme(
    axis.title.x = element_text(color="grey30", size=12),
    axis.title.y = element_text(color="grey30", size=12),
    axis.text.x = element_text(face="bold", color="grey30", size=11),
    axis.text.y = element_text(face="bold", color="grey30", size=11)
  )

dist_dkdeg <- ggplot(data = auxdf, aes(x = kx*alfa, y = ky)) + geom_point(color = "turquoise4") +
  geom_point(aes(x = dx, y = dy), color = "red")+
  scale_x_log10() + scale_y_log10() + xlab("degree scale") + ylab("Cumulative distribution") +
  theme_bw() + ggtitle(red_name) +
  theme(
    axis.title.x = element_text(color="grey30", size=12),
    axis.title.y = element_text(color="grey30", size=12),
    axis.text.x = element_text(face="bold", color="grey30", size=11),
    axis.text.y = element_text(face="bold", color="grey30", size=11)
  )

# ppi <- 300
# png("ESTATICA_hist_degrees.png", width=(12*ppi), height=4*ppi, res=ppi)
# grid.arrange(histo_core,histo_dist,histo_deg,ncol=3, nrow=1, widths=c(1/3,1/3,1/3))
# dev.off()