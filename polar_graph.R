library(scales)
library(grid)
library(gridExtra)
source("network-kanalysis.R")
paint_kdegree_kdistance <- function(graph, num_guild_a, num_guild_b, showtext = "no",
                                    network_name = "", NODF = 0, MeanKdistance = 0, printable_range = 0)
{
  g <- V(graph)
  nga <- sum(g[1:num_guild_a]$kdistance != Inf)
  ngb <- sum(g[num_guild_a+1:length(g)]$kdistance != Inf)
  g <- g[g$kdistance != Inf]
  dfaux <- data.frame(g$kdistance,g$kdegree,g$kcorenum,(g$kdegree/max(g$kdegree))^1.5)
  names(dfaux) <- c("kdistance","kdegree","kcorenum","normdegree")
  scale_factor <- 20
  dfaux$kradio <- scale_factor*sqrt(dfaux$kdegree/pi)
  dfaux$posx <- NA
  dfaux$posy <- NA
  dfaux$name <- NA
  dfaux$symbol <- 1
  dfaux$kcol_label <- NA
  dfaux$despl <- 1.2
  dfaux$name <- as.character(g$name)
  signo <- 1
  guarda <- 0.25
  min_radius <- 0
  tot_species <- nrow(dfaux)
  rndmult <- runif(tot_species,guarda,pi-guarda)
  rndmult_a <- sample(seq(guarda,pi-guarda,length.out=nga))
  rndmult_b <- sample(seq(guarda,pi-guarda,length.out=ngb))
  dfaux <- dfaux[dfaux$kdistance != Inf,]
  maxcore <- max(dfaux$kcorenum)
  extreme <- ceiling(max(dfaux[dfaux$kdistance != Inf,]$kdistance))
  num_central <- (nga+ngb)%/%5
  more_central_nodes <- head(dfaux[order(dfaux$kdistance),]$name, num_central)
  slice_multiplier <- 4
  rnd_central <- seq(guarda,pi-guarda,length.out = num_central*slice_multiplier)
  pal <-colorRampPalette(c("cadetblue","darkorchid4"))
  #vcols <- pal(maxcore)
  vcols <- pal(max(8,maxcore))
  alpha_level <- 0.3
  k <- 1
  if (printable_range >0){
    tailp <- 3
    sort_distances <- dfaux[order(dfaux$kdistance),]$name
    printable_points <- c(head(sort_distances,printable_range), tail(sort_distances,tailp))
  }
  for (i in 1:tot_species){
    if (length(which(printable_points == dfaux[i,]$name)) > 0)
      dfaux[i,]$kcol_label <- vcols[dfaux[i,]$kcorenum]
    if (i>nga)
    {
      offset <- pi
      dfaux[i,]$symbol <- 21
      rndmult <- rndmult_b[i-nga]
    }
    else{
      offset <- 0
      dfaux[i,]$symbol <- 20
      rndmult <- rndmult_a[i]
    }
    if (length(which(more_central_nodes == dfaux[i,]$name)) > 0){
      rdnmult <- rnd_central[ceiling(k*runif(1,slice_multiplier,slice_multiplier*1.5)*i%/%length(rnd_central))]
      k <- k + 1
    }
    if (dfaux[i,]$kdistance != Inf){
      dfaux[i,]$posy <- dfaux[i,]$kdistance
      dfaux[i,]$posx <- rndmult + offset
    }
    else{
      dfaux[i,]$posx <- 0
      dfaux[i,]$posy <- 0
      dfaux[i,]$kdegree <- 0.0001
    }
  }
  dfaux$fillcol <- 1 + maxcore - dfaux$kcorenum
  polar_plot <- ggplot(dfaux, aes(x=posx,y=posy),legendTextFont=c(15, "bold.italic", "red")) +
    scale_size_area(max_size=scale_factor,name="k-degree") +  
    scale_colour_manual(values = vcols,name="k-shell") +
    guides(col = guide_legend(override.aes = list(shape = 15, size = 8)), 
           shape = guide_legend(override.aes = list(size = 8, colour = "slategray1")),
           kdegree = guide_legend(override.aes = list(shape = 15, size = 8, colour = "slategray1")))
  if (showtext == "yes"){
    polar_plot <- polar_plot+ geom_text(aes(size=0.005+0.1*normdegree,angle=0,colour = factor(kcorenum), label = name), alpha = alpha_level+0.1)+
      scale_shape_identity()
  }
  else{
    polar_plot <- polar_plot + geom_point(aes(size=kdegree, colour = factor(kcorenum), shape = factor(symbol)), alpha = alpha_level) +
      geom_point(aes(size=kdegree, color=factor(kcorenum), shape = factor(symbol)),alpha = alpha_level) +
      scale_shape_manual(values=c(16,15),name="Guild",labels=c("Plant", "Pollinator") ) +
      annotate(geom="text", x=dfaux$posx, y=dfaux$posy, label=dfaux$name, colour = factor(dfaux$kcol_label), size=2*(1.8+5*dfaux$normdegree), hjust = 1, alpha = 1, guide =FALSE)
  }
  polar_plot <- polar_plot + coord_polar(start = -pi/2) + labs(x = '', y = '')# + theme(axis.text.x = element_blank()) 
  polar_plot <- polar_plot + scale_y_continuous(breaks=seq(min_radius,extreme), lim=c(min_radius, extreme),labels=seq(min_radius,extreme) )
  polar_plot <- polar_plot + scale_x_continuous(breaks=seq(0, 2*pi, by=pi/2), lim=c(0,2*pi))
  polar_plot <- polar_plot+ theme_bw() + theme(panel.border = element_blank(),
                                               legend.key = element_blank(),
                                               axis.ticks.y = element_blank(),
                                               axis.ticks.x = element_blank(),
                                               panel.grid.major.x = element_blank(),
                                               panel.grid.minor.x = element_blank(),
                                               axis.text.y = element_blank(),
                                               panel.grid.major.y = element_line(size = 0.5, linetype = 3, color="grey90"),
                                               panel.grid.minor.y = element_blank(),
                                               panel.border = element_blank(),
                                               axis.text.x = element_blank(),
                                               legend.text = element_text(size=10),
                                               plot.title = element_text(lineheight=.8, face="bold")
  )
  ylab <- seq(0,extreme)
  pylab <- ylab
  pylab[2:length(pylab)] <- pylab[2:length(pylab)]-0.05
  ylab[1] <- "k-distance"
  xlab <- rep(pi,length(pylab))
  dftext <- data.frame(xlab,ylab,pylab)
  dftext$fillcol <- maxcore
  polar_plot <- polar_plot + annotate(geom="text",x=xlab,y=pylab,label=ylab,size=4, color="gray50", lineheight=.8)
  polar_plot <- polar_plot + ggtitle(sprintf("Network %s NODF: %.02f Average k-distance: %.02f", network_name, NODF, MeanKdistance)) +
    guides(row = guide_legend(nrow = 1))
  histo_dist <- ggplot(dfaux, aes(kdistance)) + geom_histogram(alpha = alpha_level,binwidth=extreme/30, color="white",fill = "green", main = "k-distance") +
    xlim(0,extreme) +
    theme_bw() +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="gray70"),
          panel.border = element_blank(),
          legend.text = element_text(size=10),
          plot.title = element_text(lineheight=.8, face="bold"),
          axis.title.x = element_blank()
    )+
    ggtitle("k-distance") + ylab("Species")
  histo_core <- ggplot(dfaux, aes(x=kcorenum)) + geom_histogram(width = 0.5, aplha =alpha_level, binwidth=1,color="white",fill = "Light Blue") + theme(legend.position = "none") +theme_bw() +
    #xlim(1, max(dfaux$maxcore)) +
    scale_x_continuous(breaks=seq(1, maxcore, by=1), lim=c(1,maxcore+1)) +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="gray90"),
          panel.border = element_blank(),
          legend.text = element_text(size=10),
          plot.title = element_text(lineheight=.8, face="bold"),
          axis.text.x = element_text(angle = 0, hjust = -2.5),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()
    ) +
    ggtitle("k-cores")+ ylab("Species")
  histo_degree <- ggplot(dfaux, aes(kdegree)) + geom_histogram(alpha = alpha_level,binwidth=max(dfaux$kdegree)/30,
                                                               color="white",fill = "grey20", main = "k-degree") +
    xlim(0,ceiling(max(dfaux$kdegree))) +
    theme_bw() +
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="gray90"),
          panel.border = element_blank(),
          legend.text = element_text(size=10),
          plot.title = element_text(lineheight=.8, face="bold"),
          axis.title.x = element_blank()
    )+
    ggtitle("k-degree")+ ylab("Species")
  calc_grafs <- list("polar_plot" = polar_plot, "histo_dist" = histo_dist, "histo_core" = histo_core,
                     "histo_degree" = histo_degree)
  return(calc_grafs)
}
directorystr <- "data/"
red <- "M_PL_019.csv"
red_name <- strsplit(red,".csv")[[1]][1]
result_analysis <- analyze_network(red, directory = directorystr, guild_a = "pl", guild_b = "pol", plot_graphs = TRUE)
numlinks <- result_analysis$links
print_to_file <- FALSE
if (print_to_file){
  ppi <- 600
  png(paste0(red_name,"_polar.png"), width=16*ppi, height=9*ppi, res=ppi)
}
r <- paint_kdegree_kdistance(result_analysis$graph, result_analysis$num_guild_a,
                             result_analysis$num_guild_b, network_name = red_name, NODF = result_analysis$nested_values["NODF"],
                             MeanKdistance = result_analysis$meandist, showtext = "no", printable_range = 3)
grid.arrange(r["polar_plot"][[1]],arrangeGrob(r["histo_dist"][[1]], r["histo_degree"][[1]], r["histo_core"][[1]],ncol=1, nrow=3, heights=c(1/3,1/3,1/3)), ncol=2, widths=c(3/4,1/4))
if (print_to_file)
  dev.off()