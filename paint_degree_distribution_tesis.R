library(grid)
library(gridExtra)
library(stargazer)
library(igraph)
library(ggplot2)

source("network-kanalysis.R")


gen_deg_distribution <- function(red, seq_breaks = seq(1,26, by=5))
{
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
  intercept <- lm(ddegree ~ kdegree)$coefficients[1]
  
  
  red_name <- strsplit(red,".csv")[[1]][1]
  
  auxdf <- data.frame(dx,dy,kx,ky)
  dist_deg <- ggplot(data = auxdf, aes(x = dx, y = dy)) + geom_point(color = "red", alpha = 0.8, shape=1) +
    scale_x_log10(breaks = seq_breaks) + scale_y_log10() + xlab("degree") + ylab("Cumulative distribution") + ggtitle("") +
    theme_bw() + 
    theme(
      axis.title.x = element_text(color="grey30", size=13),
      axis.title.y = element_text(color="grey30", size=13),
      axis.text.x = element_text(face="bold", color="grey30", size=10),
      axis.text.y = element_text(face="bold", color="grey30", size=10)
    )

  dist_kdeg <- ggplot(data = auxdf, aes(x = kx, y = ky)) + geom_point(color = "darkgreen", alpha = 0.8, shape=1) +
    scale_x_log10(breaks = seq_breaks) + scale_y_log10() + xlab("k degree") + ylab("") + ggtitle(red_name) +
    theme_bw() + 
    theme(
      axis.title.x = element_text(color="grey30", size=13),
      axis.title.y = element_text(color="grey30", size=13),
      axis.text.x = element_text(face="bold", color="grey30", size=10),
      axis.text.y = element_text(face="bold", color="grey30", size=10)
    )
  
  dist_dkdeg <- ggplot(data = auxdf, aes(x = kx*alfa , y = ky)) + geom_point(color = "turquoise4", alpha = 0.8, shape=1) +
    geom_point(aes(x = dx, y = dy), color = "red", alpha = 0.8, shape=1) +
    scale_x_log10(breaks = seq_breaks) + scale_y_log10() + xlab("degree scale") + ylab("") + ggtitle("") +
    theme_bw() +
    theme(
      axis.title.x = element_text(color="grey30", size=13),
      axis.title.y = element_text(color="grey30", size=13),
      axis.text.x = element_text(face="bold", color="grey30", size=10),
      axis.text.y = element_text(face="bold", color="grey30", size=10)
    )
  calc_values <- list("dist_deg" = dist_deg, "dist_kdeg" = dist_kdeg, "dist_dkdeg" = dist_dkdeg)
  return(calc_values)
}

red <- "M_PL_001"

grafs <- gen_deg_distribution(paste0(red,".csv"))

ppi <- 300
png(paste0("graphs/kdegree_over_degree_",red,".png"), width=(12*ppi), height=4*ppi, res=ppi)
grid.arrange(grafs$dist_deg,grafs$dist_kdeg,grafs$dist_dkdeg, ncol=3, nrow=1, widths=c(1/3,1/3,1/3) )
dev.off()