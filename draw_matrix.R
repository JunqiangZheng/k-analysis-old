library(scales)
# library(gplots)
library("ggplot2")
library("plyr")
library("reshape2")
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

source("network-kanalysis.R")
network_name <- "M_SD_016"
result_analysis <- analyze_network(paste0(network_name,".csv"), directory = "data/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = TRUE)


orderbykcorenumkdegree <- TRUE
put_title <- TRUE


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

if (nrow(interaction_mat.s)<ncol(interaction_mat.s)){
  if (orderbykcorenumkdegree){
    interaction_mat.s$Name <- factor(interaction_mat.s$Name,
                                     levels=interaction_mat.s$Name[rev(order(interaction_mat.s$kcorenum_a,
                                                                             interaction_mat.s$kdegree_a))])
    interaction_mat.s$variable <- factor(interaction_mat.s$variable,
                                         levels=interaction_mat.s$variable[order(interaction_mat.s$kcorenum_b,
                                                                                 interaction_mat.s$kdegree_b)])
  }
  vec_colors_y <- interaction_mat.s$vcols_b
  vec_colors_x <- interaction_mat.s$vcols_a
  col_text_y <- "firebrick2"
  col_text_x <- "slateblue2"
  p <- ggplot(interaction_mat.s, aes(variable,Name))
  
} else {
  if (orderbykcorenumkdegree){
    interaction_mat.s$Name <- factor(interaction_mat.s$Name,
                                     levels=interaction_mat.s$Name[rev(order(interaction_mat.s$kcorenum_b,
                                                                             interaction_mat.s$kdegree_b))])
    interaction_mat.s$variable <- factor(interaction_mat.s$variable,
                                     levels=interaction_mat.s$variable[order(interaction_mat.s$kcorenum_a,
                                                                       interaction_mat.s$kdegree_a)])
  }
#   if (orderbykdegree){
#     interaction_mat.s$Name <- factor(interaction_mat.s$Name,levels=interaction_mat.s$Name[rev(order(interaction_mat.s$kdegree_b))])
#     interaction_mat.s$variable <- factor(interaction_mat.s$variable,levels=interaction_mat.s$variable[order(interaction_mat.s$kdegree_a)])
#   }
  vec_colors_y <- interaction_mat.s$vcols_a
  vec_colors_x <- interaction_mat.s$vcols_b
  col_text_x <- "firebrick2"
  col_text_y <- "slateblue2"
  p <- ggplot(interaction_mat.s, aes(Name,variable))
  
  
}


p<- p +
  geom_tile(aes(fill = kcoremeasure), colour = "white") + 
  scale_fill_gradient(low = "white", high = "darkorchid4") + 
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_bw(base_size = 10) + 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.y = element_text(angle = 0 , hjust = 0, face="bold", color = col_text_y),
        axis.text.x = element_text(angle = 270 , hjust= 0, vjust = 1,  face="bold" , color = col_text_x )
        )


if (put_title)
  p <- p + ggtitle(sprintf("%s NODF: %.02f Modularity: %.04f Average K-distance: %.02f\n", network_name, NODF, Modularity, MeanKdistance))

print(p)
