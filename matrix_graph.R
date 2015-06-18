library(scales)
library("ggplot2")
library("plyr")
library("reshape2")
source("network-kanalysis.R")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


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

draw_kline <- function(p,kcorenum,xpos,ylim,ptext,vertical = TRUE, pcolor="grey", printline = TRUE, minkcorey = 0, lsize = 10)
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
  plabel <- paste0("k",kcorenum)
  if (printline)
    p <- p + geom_line(data=ds,aes(x=x,y=y),color=pcolor,linetype="dashed")
  if (vertical)
      p <- p+ geom_text(data=NULL,x=ptext,y=0.25+0.25*as.integer(ylim>10)+0.01*as.integer(ylim),
                        vjust=1,label=plabel, cex=3, color = pcolor,  fontface="italic", size = lsize)
  else{
      #py <- ifelse(kcorenum != minkcorey,ptext+0.5+as.integer(ylim>20)*0.015*as.integer(ylim),1.2)
      py <- ifelse(kcorenum != minkcorey,ptext+0.6,0.5)
      p <- p+ geom_text(data=NULL,y=py,x=ylim+0.2,hjust=0,vjust=0,label=plabel, 
                        angle=90, cex=3, color = pcolor, fontface="italic", size = lsize)
  }
  return(p)
}

matrix_graph <- function(  network_name,
                                      orderbykcorenumkdegree = TRUE,
                                      put_title = TRUE,
                                      lsize_axis = 10,
                                      lsize_kcorenum = 8,
                                      plotsdir = "plot_results/matrix",
                                      dirdata = "data/",
                                      printfile = TRUE,
                                      label_strguilda = "Plants", 
                                      label_strguildb = "Pollinators",
                                      color_guild_a = c("#4169E1","#00008B"), 
                                      color_guild_b = c("#F08080","#FF0000")
                                      )




{
  if (nchar(label_strguilda)>0){
    slabels <- c(label_strguilda, label_strguildb)
    name_guild_a <- label_strguilda
    name_guild_b <- label_strguildb
  }
  result_analysis <- analyze_network(paste0(network_name,".csv"), directory = dirdata,
                                     guild_a = name_guild_a, guild_b = name_guild_b, plot_graphs = TRUE)
  interaction_mat <- as.data.frame(result_analysis$matrix)
  dropchars <- "[\\.,-]"
  names_a <- trim(gsub(dropchars," ", colnames(interaction_mat)))
  numspecies_a <- length(names_a)
  names_b <- trim(gsub(dropchars," ", rownames(interaction_mat)))
  numspecies_b <- length(names_b)
  kdegree_a <- rep(0,ncol(interaction_mat))
  kdegree_b <- rep(0,nrow(interaction_mat))
  kcorenum_a <- rep(0,ncol(interaction_mat))
  kcorenum_b <- rep(0,nrow(interaction_mat))
  V(result_analysis$graph)$name_species <- trim(gsub(dropchars," ",V(result_analysis$graph)$name_species))
  for (i in 1:ncol(interaction_mat))
  {
    kdegree_a[i] <- V(result_analysis$graph)[V(result_analysis$graph)$name_species == names_a[i]]$kdegree
    kcorenum_a[i] <- V(result_analysis$graph)[V(result_analysis$graph)$name_species == names_a[i]]$kcorenum
  }
  for (i in 1:nrow(interaction_mat))
  {
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
    if (interaction_mat.s$kcoremeasure[i]>0)
      interaction_mat.s$kcoremeasure[i] <- interaction_mat.s$kcoremeasure[i] + 4
  }
  
  for (i in 1:nrow(interaction_mat.s)){
    interaction_mat.s$Name[i] <- paste(which(names_b==interaction_mat.s$Name[i]),interaction_mat.s$Name[i])
    interaction_mat.s$variable[i] <- paste(which(names_a==interaction_mat.s$variable[i]),interaction_mat.s$variable[i])
  }
  
  interaction_mat.s$vcols_a <- 0
  interaction_mat.s$vcols_b <- 0
  
  maxcore <- max(kcorenum_a,kcorenum_b)
  pal_a <-colorRampPalette(color_guild_b)
  vcols_a <- pal_a(maxcore)
  pal_b <-colorRampPalette(color_guild_a)
  vcols_b <- pal_b(maxcore)
  for (i in 1:nrow(interaction_mat.s))
  {
    interaction_mat.s$vcols_a[i]<- vcols_a[interaction_mat.s$kcorenum_a[i]]
    interaction_mat.s$vcols_b[i]<- vcols_b[interaction_mat.s$kcorenum_b[i]]
  }
  
  if (nrow(interaction_mat)<ncol(interaction_mat)){  # More  plants than animals
    print("mas plantas que bichos")
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
    numspecies_y <- numspecies_b
    numspecies_x <- numspecies_a
    col_text_y <- color_guild_b[1]
    col_text_x <- color_guild_a[1]
    p <- ggplot(interaction_mat.s, aes(variable,Name))
    species_axis_y <- rev(unique(levels(interaction_mat.s$Name)))
    
    kcorenum_axis_y <- c()
    for (i in 1:length(species_axis_y)){
      kcorenum <- interaction_mat.s[interaction_mat.s$Name == species_axis_y[i],]$kcorenum_b[1]
      kcorenum_axis_y <- c(kcorenum_axis_y,kcorenum) 
    }
    #Kcorenum_axis_y <- rev(kcorenum_axis_y)
    species_axis_x <- unique(levels(interaction_mat.s$variable))
    kcorenum_axis_x <- c()
    for (i in 1:length(species_axis_x)){
      kcorenum <- interaction_mat.s[interaction_mat.s$variable == species_axis_x[i],]$kcorenum_a[1]
      kcorenum_axis_x <- c(kcorenum_axis_x,kcorenum) 
    }
    pcolory=color_guild_b[1]
    pcolorx=color_guild_a[1]

    
  } else {
    print("mas bichos que plantas")
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
    numspecies_y <- numspecies_a
    numspecies_x <- numspecies_b
    col_text_x <- color_guild_b[1]
    col_text_y <- color_guild_a[1]
    p <- ggplot(interaction_mat.s, aes(Name,variable))
    
    species_axis_y <- rev(unique(levels(interaction_mat.s$variable)))
    #species_axis_y <- unique(levels(interaction_mat.s$variable))
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
    pcolorx=color_guild_b[1]
    pcolory=color_guild_a[1]
  }
  
  changes_x <- changes_kcore(kcorenum_axis_x)
  changes_y <- changes_kcore(kcorenum_axis_y)
  lsize <- lsize_axis - min(4,0.33*as.integer(max(numspecies_x%/%40)))
  
  p<- p +
    geom_tile(aes(fill = kcoremeasure), colour = "white") + 
    scale_fill_gradient(low = "white", high = "darkmagenta") + 
    scale_x_discrete("", expand = c(0, 0)) + 
    scale_y_discrete("", expand = c(0, 0)) + 
    theme_bw(base_size = lsize) + 
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
    p <- draw_kline(p,changes_x$kcorenum[i],changes_x$species[i], length(species_axis_y), 
                    0.5+(changes_x$species[i]+anterior)/2 ,pcolor=pcolorx, printline = (i!=nrow(changes_x)),
                    lsize = lsize_kcorenum)

    anterior <- changes_x$species[i]
  }
  

  for (i in 1:nrow(changes_y))
    p <- draw_kline(p,changes_y$kcorenum[i],numspecies_y-changes_y$species[i], length(species_axis_x) ,
                    numspecies_y-changes_y$species[i],vertical = FALSE, pcolor=pcolory, printline = (i!=nrow(changes_y)),
                    minkcorey = min(changes_y$kcorenum),  lsize = lsize_kcorenum)
    
  
  if (put_title)
    p <- p + ggtitle(sprintf("%s NODF: %.02f Modularity: %.04f Average K-distance: %.02f\n", network_name, NODF, Modularity, MeanKdistance))
  
  if (printfile){
    dir.create(plotsdir, showWarnings = FALSE)
    png(paste0("",plotsdir,"/",network_name,"_matrix.png"), width=(4000), height=2200+as.integer(numspecies_y>20)*(numspecies_y%/%10)*100, res=300)
  }
  print(p)
  if (printfile)
    dev.off()
}

#matrix_graph("M_PL_016")