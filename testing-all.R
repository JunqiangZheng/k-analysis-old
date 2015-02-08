library(scales)
source("network-kanalysis.R")
library(grid)

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
  dfaux$symbol <- 0
  dfaux$kcol_label <- NA
  dfaux$name <- as.character(g$name)
  signo <- 1
  guarda <- 0.15
  tot_species <- nrow(dfaux)
  rndmult <- runif(tot_species,guarda,pi-guarda)
  rndmult_a <- sample(seq(guarda,pi-guarda,length.out=nga))
  rndmult_b <- sample(seq(guarda,pi-guarda,length.out=ngb))
  dfaux <- dfaux[dfaux$kdistance != Inf,]
  maxcore <- max(dfaux$kcorenum)
  extreme <- ceiling(max(dfaux[dfaux$kdistance != Inf,]$kdistance))
  if (printable_range >0){
    sort_distances <- dfaux[order(dfaux$kdistance),]$name
    printable_points <- c(head(sort_distances,printable_range), tail(sort_distances,printable_range))
  }
  for (i in 1:tot_species){
    if (length(which(printable_points == dfaux[i,]$name)) > 0)
      dfaux[i,]$kcol_label <- dfaux[i,]$kcorenum
    if (i>nga)
    {
      offset <- pi
      dfaux[i,]$symbol <- 1
      rndmult <- rndmult_b[i-nga]
    }
    else{
      offset <- 0
      rndmult <- rndmult_a[i]
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
  pal <-colorRampPalette(c("Midnight Blue","Red"))
  vcols <- pal(maxcore)
  min_radius <- 0
  dfaux$fillcol <- 1 + maxcore - dfaux$kcorenum
  mplot <- ggplot(dfaux, aes(x=posx,y=posy),legendTextFont=c(15, "bold.italic", "red")) +
          #geom_point(aes(size=kdegree, colour = factor(kcorenum), shape = factor(symbol)), alpha = 0.2)+
          scale_size_area(max_size=scale_factor,name="k-degree") +
          #scale_shape_manual(values=c(16,15),name="Guild",labels=c("Plant", "Pollinator"))+
          scale_colour_manual(values = vcols,name="k-shell") +
  guides(col = guide_legend(override.aes = list(shape = 15, size = 10)))
  if (showtext == "yes"){
    mplot <- mplot + geom_text(aes(size=0.1*normdegree,angle=0,colour = factor(kcorenum), label = name), alpha = 0.2)+
             scale_shape_identity() 
  }
  else{
    mplot <- mplot + geom_point(aes(size=kdegree, colour = factor(kcorenum), shape = factor(symbol)), alpha = 0.2) +
             scale_shape_manual(values=c(16,15),name="Guild",labels=c("Plant", "Pollinator")) +
             geom_text( mapping=aes(label=name, colour = factor(kcol_label), size=1+2*normdegree), hjust = 1.2)
  }
    
#   mplot <- mplot + scale_colour_gradientn(colours=vcols,na.value = "transparent",
#                                           breaks=c(1,1,maxcore),labels=c(1,1,maxcore),
#                                           limits=c(1,maxcore))
#   mplot <- mplot + geom_point( size = scale_factor*sqrt(dfaux$kdegree)/pi, pch = dfaux$symbol, alpha = 0.2)  
  mplot <- mplot +  coord_polar(start = -pi/2) + theme(axis.text.x = element_blank()) + labs(x = '', y = '')
  mplot <- mplot + scale_y_continuous(breaks=seq(min_radius,extreme), lim=c(min_radius, extreme),labels=seq(min_radius,extreme) )
  mplot <- mplot + scale_x_continuous(breaks=seq(0, 2*pi, by=pi/2), lim=c(0,2*pi)) 
#   if (showtext == "yes")
#     mplot <- mplot + geom_text(data=dfaux, mapping=aes(x=posx, y=posy, label=name), size=3)
  mplot <- mplot+ theme_bw() + theme(panel.border = element_blank(),
                                     legend.key = element_blank(),
                                     axis.ticks.y = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     panel.grid.major.x = element_blank(),
                                     panel.grid.minor.x = element_blank(),
                                     axis.text.y = element_blank(),
                                     panel.grid.major.y = element_line(linetype = 2, color="Gray80"),
                                     panel.grid.minor.y = element_blank(),
                                     axis.text.x = element_blank(),
                                     legend.text = element_text(size=10))

#   ylab <- seq(1,extreme)
#   xlab <- rep(pi,extreme)
#   dftext <- data.frame(xlab,ylab)
#   dftext$fillcol <- maxcore
#   mplot <- mplot + geom_text(data = dftext,mapping = aes(x=xlab,y=-0.01+ylab,label=ylab), size=4)
#   ylab <- seq(1,extreme)
#   xlab <- rep(pi,extreme)
#   dftext <- data.frame(xlab,ylab)
#   dftext$fillcol <- maxcore
#   mplot <- mplot + geom_text(data = dftext,mapping = aes(x=xlab,y=-0.01+ylab,label=ylab), size=4)
#   mplot <- mplot + ggtitle(sprintf("Network:%s  NODF:%.02f  Average k-distance:%.02f", network_name, NODF, MeanKdistance))
  return(mplot)
}

red <- "M_PL_016.csv"
result_analysis <- analyze_network(red, directory = "data/", guild_a = "pl", guild_b = "pol", plot_graphs = TRUE)

#abort()

numlinks <- result_analysis$links
vecnames <- c("Network","Plants","Pollinators","Interactions","MaxKcore","MeanKdegree","MeanKdistance","MaxKdistance","NODF","Cscore","RemovedLinks") #"wine","Cscore")
resultdf <- data.frame(matrix(ncol = length(vecnames), nrow = 0))
names(resultdf) <- vecnames

directorystr <- "data/"

analizatodo <- FALSE
#randomize <- FALSE
randomize <- TRUE
numexper <- 2
wipedperc <- 0.10

vnodf <- rep(0,numexper)
vkdist <- rep(0,numexper)

if(analizatodo)
{
  indexrow <- 1
  if (!randomize)
    numexper = 1
  for (e in 1:numexper)
  {  
    print(paste0("EXPERIMENTO",e))
    if (randomize){
      directorystr <- "datarnd/"
      wipelinks <- seq(1,max(10,round(numlinks*wipedperc)))
      for (qlinks in wipelinks)
      {
        randomize_and_write(result_analysis$matrix,red, bypercentage = TRUE,directory ="datarnd/", rlinks = qlinks)
      }
      nfiles <- Sys.glob(paste0(directorystr,paste0(strsplit(red,"\\.")[[1]][1],"*.csv")))
    }
    else
      nfiles <- Sys.glob(paste0(directorystr,"M*.csv"))
    for (l in nfiles)
    {
      print(l)
      namefile <- strsplit(l,"/")
      namenetwork <- namefile[[1]][2]
      resultdf[indexrow,]$RemovedLinks <- strsplit(strsplit(namefile[[1]][2],"\\.")[[1]][1],"_rnd_")[[1]][2]
      result_analysis <- analyze_network(namenetwork, directory = directorystr, guild_a = "pl", guild_b = "pol", plot_graphs = FALSE)
      #print(V(result_analysis$graph))
      resultdf[indexrow,]$Network <- namenetwork
      resultdf[indexrow,]$Plants <- result_analysis$num_guild_a
      resultdf[indexrow,]$Pollinators <- result_analysis$num_guild_b
      resultdf[indexrow,]$Interactions <- length(E(result_analysis$graph))
      resultdf[indexrow,]$MaxKcore <- result_analysis$max_core
      distances <- V(result_analysis$graph)$kdistance
      resultdf[indexrow,]$MaxKdistance <- max(distances[distances!=Inf])
      if (is.na(resultdf[indexrow,]$MeanKdistance)){
        resultdf[indexrow,]$MeanKdistance <- result_analysis$meandist
      }
      else
      {
        resultdf[indexrow,]$MeanKdistance <- resultdf[indexrow,]$MeanKdistance + result_analysis$meandist
      }
      resultdf[indexrow,]$MeanKdistance <- resultdf[indexrow,]$MeanKdistance + result_analysis$meandist
      resultdf[indexrow,]$MeanKdegree <- result_analysis$meankdegree
      if (is.na(resultdf[indexrow,]$NODF)){
        resultdf[indexrow,]$NODF <- result_analysis$nested_values["NODF"]
      }
      else
      {
        resultdf[indexrow,]$NODF <- resultdf[indexrow,]$NODF + result_analysis$nested_values["NODF"]
      }
      #resultdf[indexrow,]$wine <- result_analysis$nested_values["wine"]
      resultdf[indexrow,]$Cscore <- result_analysis$nested_values["C.score"]
      indexrow <- indexrow +1 
    }
  }
  
  p <- qplot(MeanKdistance, NODF, data = resultdf, color = RemovedLinks)+scale_colour_hue(h=c(180, 360))
  print(p)
  q <- qplot(log(MeanKdistance), log(NODF), data = resultdf, color = RemovedLinks)+scale_colour_hue(h=c(180, 360))
  print(q)
  write.csv(resultdf,"datos_analisis.csv", row.names=FALSE)
}

r <- paint_kdegree_kdistance(result_analysis$graph, result_analysis$num_guild_a, 
                             result_analysis$num_guild_b, network_name = strsplit(red,".csv")[[1]][1], NODF = result_analysis$nested_values["NODF"],
                             MeanKdistance = result_analysis$meandist, showtext = "no", printable_range = 10)
print(r)