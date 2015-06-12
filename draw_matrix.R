library(scales)
# library(gplots)
library("ggplot2")
library("plyr")
library("reshape2")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

source("network-kanalysis.R")
orderbykcorenumkdegree <- TRUE
put_title <- TRUE
lsize_axis <- 12

network_name <- "M_SD_013"
result_analysis <- analyze_network(paste0(network_name,".csv"), directory = "data/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = TRUE)

changes_kcore <- function(kcorevector)
{
  auxdf <- data.frame(c())
  kcoreact <- kcorevector[1]
  for (i in 2:length(kcorevector))
    if (kcoreact != kcorevector[i]){
      auxdf <- rbind(auxdf,c(kcoreact,i-1))
      kcoreact = kcorevector[i]
    }
  auxdf <- rbind(auxdf,c(kcoreact,i))
  names(auxdf) <- c("kcorenum","species")
  return(auxdf)
}

draw_kline <- function(p,kcorenum,xpos,ylim,ptext,vertical = TRUE, pcolor="grey")
{
  if (vertical) {
  x <- c(xpos+0.5,xpos+0.5)
  y <- c(-0.25,ylim+0.5)
  }
  else{
  y <- c(xpos+0.5,xpos+0.5)
  x <- c(0,ylim+0.5)
  }
    
  ds <- data.frame(x,y)
  p <- p + geom_line(data=ds,aes(x=x,y=y),color=pcolor,linetype="dashed")
  if (vertical)
      p <- p+ geom_text(data=NULL,x=ptext,y=0.5+0.5*as.integer(ylim>10),vjust=1,label=paste0(kcorenum,"k"), cex=3, color = pcolor,  fontface="italic")
  else
    p <- p+ geom_text(data=NULL,y=ptext,x=ylim+0.2,hjust=0.5,vjust=.5,label=paste0(kcorenum,"k"), angle=270, cex=3, color = pcolor, fontface="italic")
  return(p)
}

interaction_mat <- as.data.frame(result_analysis$matrix)
dropchars <- "[\\.,-]"
names_a <- trim(gsub(dropchars," ", colnames(interaction_mat)))
names_b <- trim(gsub(dropchars," ", rownames(interaction_mat)))
kdegree_a <- rep(0,ncol(interaction_mat))
kdegree_b <- rep(0,nrow(interaction_mat))
kcorenum_a <- rep(0,ncol(interaction_mat))
kcorenum_b <- rep(0,nrow(interaction_mat))
V(result_analysis$graph)$name_species <- trim(gsub(dropchars," ",V(result_analysis$graph)$name_species))
for (i in 1:ncol(interaction_mat))
{
  print(names_a[i])
  kdegree_a[i] <- V(result_analysis$graph)[V(result_analysis$graph)$name_species == names_a[i]]$kdegree
  kcorenum_a[i] <- V(result_analysis$graph)[V(result_analysis$graph)$name_species == names_a[i]]$kcorenum
}
for (i in 1:nrow(interaction_mat))
{
  print(names_b[i])
  kdegree_b[i] <- V(result_analysis$graph)[V(result_analysis$graph)$name_species == names_b[i]]$kdegree
  kcorenum_b[i] <- V(result_analysis$graph)[V(result_analysis$graph)$name_species == names_b[i]]$kcorenum
}

NODF <- result_analysis$nested_values["NODF"]
Modularity <-  result_analysis$modularity_measure
MeanKdistance <- result_analysis$meandist

interaction_mat$Name <- rownames(interaction_mat)
interaction_mat$Name <- trim(gsub(dropchars," ", interaction_mat$Name))

interaction_mat.m <- melt(interaction_mat)
interaction_mat.s <- ddply(interaction_mat.m, .(variable), transform,
               rescale = scale(value))
interaction_mat.s$variable <- as.character(trim(gsub(dropchars," ", interaction_mat.s$variable)))
interaction_mat.s$value <- as.integer(interaction_mat.s$value>0)
interaction_mat.s$rescale <-interaction_mat.s$value
interaction_mat.s$kdegree_b <- 0
interaction_mat.s$kcorenum_b <- 0
interaction_mat.s$kdegree_a <- 0
interaction_mat.s$kcorenum_a <- 0
interaction_mat.s$kcoremeasure <- 0
for (i in 1:nrow(interaction_mat.s)){
  interaction_mat.s$kdegree_b[i] <- kdegree_b[which(names_b==interaction_mat.s$Name[i])]
  interaction_mat.s$kcorenum_b[i] <- kcorenum_b[which(names_b==interaction_mat.s$Name[i])]
}
for (i in 1:nrow(interaction_mat.s)){
  interaction_mat.s$kdegree_a[i] <- kdegree_a[which(names_a==interaction_mat.s$variable[i])]
  interaction_mat.s$kcorenum_a[i] <- kcorenum_a[which(names_a==interaction_mat.s$variable[i])]
  interaction_mat.s$kcoremeasure[i] <- interaction_mat.s$rescale[i]*interaction_mat.s$kcorenum_a[i]*interaction_mat.s$kcorenum_b[i]
}

for (i in 1:nrow(interaction_mat.s)){
  interaction_mat.s$Name[i] <- paste(which(names_b==interaction_mat.s$Name[i]),interaction_mat.s$Name[i])
  interaction_mat.s$variable[i] <- paste(which(names_a==interaction_mat.s$variable[i]),interaction_mat.s$variable[i])
}

interaction_mat.s$vcols_a <- 0
interaction_mat.s$vcols_b <- 0

maxcore <- max(kcorenum_a,kcorenum_b)
pal_a <-colorRampPalette(c("pink1","red"))
vcols_a <- pal_a(maxcore)
pal_b <-colorRampPalette(c("steelblue2","blueviolet"))
vcols_b <- pal_b(maxcore)
for (i in 1:nrow(interaction_mat.s))
{
  interaction_mat.s$vcols_a[i]<- vcols_a[interaction_mat.s$kcorenum_a[i]]
  interaction_mat.s$vcols_b[i]<- vcols_b[interaction_mat.s$kcorenum_b[i]]
}

if (nrow(interaction_mat)<ncol(interaction_mat)){
  if (orderbykcorenumkdegree){
    interaction_mat.s$Name <- factor(interaction_mat.s$Name,
                                     levels=interaction_mat.s$Name[order(interaction_mat.s$kcorenum_b,
                                                                             interaction_mat.s$kdegree_b)])
    interaction_mat.s$variable <- factor(interaction_mat.s$variable,
                                         levels=interaction_mat.s$variable[rev(order(interaction_mat.s$kcorenum_a,
                                                                                 interaction_mat.s$kdegree_a))])
  }
  vec_colors_y <- interaction_mat.s$vcols_b
  vec_colors_x <- interaction_mat.s$vcols_a
  col_text_y <- "tomato3"
  col_text_x <- "cadetblue4"
  p <- ggplot(interaction_mat.s, aes(variable,Name))
  species_axis_y <- rev(unique(levels(interaction_mat.s$Name)))
  kcorenum_axis_y <- c()
  for (i in 1:length(species_axis_y)){
    kcorenum <- interaction_mat.s[interaction_mat.s$Name == species_axis_y[i],]$kcorenum_b[1]
    kcorenum_axis_y <- c(kcorenum_axis_y,kcorenum) 
  }
  species_axis_x <- unique(levels(interaction_mat.s$variable))
  kcorenum_axis_x <- c()
  for (i in 1:length(species_axis_x)){
    kcorenum <- interaction_mat.s[interaction_mat.s$variable == species_axis_x[i],]$kcorenum_a[1]
    kcorenum_axis_x <- c(kcorenum_axis_x,kcorenum) 
  }
  pcolory="sienna1"
  pcolorx="cadetblue3"
  
} else {
  if (orderbykcorenumkdegree){
    interaction_mat.s$Name <- factor(interaction_mat.s$Name,
                                     levels=interaction_mat.s$Name[rev(order(interaction_mat.s$kcorenum_b,
                                                                         interaction_mat.s$kdegree_b))])
    interaction_mat.s$variable <- factor(interaction_mat.s$variable,
                                         levels=interaction_mat.s$variable[order(interaction_mat.s$kcorenum_a,
                                                                                     interaction_mat.s$kdegree_a)])
  }
  vec_colors_y <- interaction_mat.s$vcols_a
  vec_colors_x <- interaction_mat.s$vcols_b
  col_text_x <- "tomato3"
  col_text_y <- "cadetblue4"
  p <- ggplot(interaction_mat.s, aes(Name,variable))
  
  species_axis_y <- rev(unique(levels(interaction_mat.s$variable)))
  kcorenum_axis_y <- c()
  for (i in 1:length(species_axis_y)){
    kcorenum <- interaction_mat.s[interaction_mat.s$variable == species_axis_y[i],]$kcorenum_a[1]
    kcorenum_axis_y <- c(kcorenum_axis_y,kcorenum) 
  }
  
  species_axis_x <- unique(levels(interaction_mat.s$Name))
  kcorenum_axis_x <- c()
  for (i in 1:length(species_axis_x)){
    kcorenum <- interaction_mat.s[interaction_mat.s$Name == species_axis_x[i],]$kcorenum_b[1]
    kcorenum_axis_x <- c(kcorenum_axis_x,kcorenum) 
  }
  pcolorx="sienna1"
  pcolory="cadetblue3"
}

changes_x <- changes_kcore(kcorenum_axis_x)
changes_y <- changes_kcore(kcorenum_axis_y)
changes_y$kcorenum <- rev(changes_y$kcorenum)

p<- p +
  geom_tile(aes(fill = kcoremeasure), colour = "white") + 
  scale_fill_gradient(low = "white", high = "darkorchid4") + 
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_bw(base_size = lsize_axis) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.y = element_text(angle = 0 , hjust = 0, face="bold", color = col_text_y),
        axis.text.x = element_text(angle = 270 , hjust= 0, vjust = 0.5,  face="bold" , color = col_text_x ),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
        )
anterior <- 1
for (i in 1:nrow(changes_x)){
  p <- draw_kline(p,changes_x$kcorenum[i],changes_x$species[i], length(species_axis_y), 0.5+(changes_x$species[i]+anterior)/2 ,pcolor=pcolorx)
  anterior <- changes_x$species[i]
}

for (i in 1:nrow(changes_y))
  p <- draw_kline(p,changes_y$kcorenum[i],changes_y$species[i], length(species_axis_x) , changes_y$species[i]-0.1,vertical = FALSE, pcolor=pcolory)

if (put_title)
  p <- p + ggtitle(sprintf("%s NODF: %.02f Modularity: %.04f Average K-distance: %.02f\n", network_name, NODF, Modularity, MeanKdistance))

print(p)
