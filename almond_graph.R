library(scales)
library(grid)
library(gridExtra)
source("network-kanalysis.R")


str_guild_a <- "pl"
str_guild_b <- "pol"
directorystr <- "data/"
red <- "M_PL_058.csv"
result_analysis <- analyze_network(red, directory = directorystr, guild_a = str_guild_a, guild_b = str_guild_b, plot_graphs = TRUE)
g <- V(result_analysis$graph)
g <- g[g$kdistance != Inf]
nodes_guild_a <- grep(str_guild_a,g$name)
nodes_guild_b <- grep(str_guild_b,g$name)
nw_guild_a <-  g[nodes_guild_a]
nw_guild_b <-  g[nodes_guild_b]
ind_cores <- unique(g$kcorenum)
kcoremax <- max(ind_cores)
species_guild_a <- rep(NA,kcoremax)
species_guild_b <- rep(NA,kcoremax)
num_species_guild_a <- rep(NA,kcoremax)
num_species_guild_b <- rep(NA,kcoremax)
df_cores <- data.frame(species_guild_a, species_guild_b, num_species_guild_a, num_species_guild_b)

ymax <- 40
hop_x <- 0.5
strips_height <- ymax/(kcoremax-2)
list_dfs_a <- list()
list_dfs_b <- list()

for (i in ind_cores) {
  df_cores[i,]$species_guild_a <- list(which(g[grep(str_guild_a,g$name)]$kcorenum == i))
  df_cores[i,]$num_species_guild_a <- length((df_cores[i,]$species_guild_a)[[1]])
  df_cores[i,]$species_guild_b <- list(which(g[grep(str_guild_b,g$name)]$kcorenum == i))
  df_cores[i,]$num_species_guild_b <- length((df_cores[i,]$species_guild_b)[[1]])
}
  

pal <-colorRampPalette(c("darkorchid1","darkorchid4"))
vcols <- pal(10)

draw_coremax_triangle <- function(basex,topx,basey,topy,numboxes,fillcolor)
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  xstep <- (topx-basex)*1/numboxes
  ystep <- (topy-basey)*0.8/numboxes
  for (j in (1:numboxes))
  {
    x1 <- c(x1, basex+(j-1)*xstep)
    x2 <- c(x2, x1[j]+0.9*xstep)
    y1 <- c(y1, basey)
    y2 <- c(y2, topy-(j-1)*ystep)
    r <- c(r,j)
  }
  d1 <- data.frame(x1, x2, y1, y2, r)
  return(d1)
}

conf_ziggurat <- function(basex,widthx,basey,widthy,numboxes,fillcolor,inverse = "no")
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  xstep <- widthx/numboxes
  ystep <- widthy/numboxes
  topx <- basex + widthx
  for (j in (1:numboxes))
  {
    x1 <- c(x1, basex+(j-1)*xstep/2)
    x2 <- c(x2, topx-(j-1)*xstep/2)
    y1 <- c(y1, basey+(j-1)*ystep)
    y2 <- c(y2, y1[j]+ystep*0.9)
    r <- c(r,j)
  }
  d1 <- data.frame(x1, x2, y1, y2, r)
  if (inverse == "yes")
  {
    d1$y1 <- rev(d1$y1)
    d1$y2 <- rev(d1$y2)
  }  
  return(d1)
}

draw_ziggurat <- function(basex = 0, widthx = 0, basey = 0, widthy = 0, numboxes = 0, colorb = "", grafo = "", zinverse ="no")
{
  dr <- conf_ziggurat(basex,widthx,basey,widthy,numboxes,colorb,inverse = zinverse)
  p <- grafo + geom_rect(data=dr, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = colorb, color="transparent", alpha=0.5) +
    geom_text(data=dr, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4)
  calc_grafs <- list("p" = p, "dr" = dr) 
  return(calc_grafs)
}

color_guild_a <- "red"
color_guild_b <- "blue"

stepx <- 0.2
num_a_coremax <- df_cores[kcoremax,]$num_species_guild_a
basex <- 0
basey <- 1.5
topxa <- stepx*num_a_coremax+basex
topy <- 5+(num_a_coremax*0.15)+basey

list_dfs_a[[kcoremax]] <- draw_coremax_triangle(basex,topxa,basey,topy,num_a_coremax,color_guild_a)
p <- ggplot() +
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=list_dfs_a[[kcoremax]], mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "red",  color="transparent",alpha=0.5) +
  geom_text(data=list_dfs_a[[kcoremax]], aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=df_cores$species_guild_a[[kcoremax]], fill = "red"), size=4)


num_b_coremax <- df_cores[kcoremax,]$num_species_guild_b
basex <- 0
basey <- -1.5
topxb <- stepx*num_b_coremax+basex
topy <- -5-(num_b_coremax*0.15)+basey

list_dfs_b[[kcoremax]] <- draw_coremax_triangle(basex,topxb,basey,topy,num_b_coremax,"blue")

p <- p + geom_rect(data=list_dfs_b[[kcoremax]] , mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "blue",  color="transparent", alpha=0.5) +
     geom_text(data=list_dfs_b[[kcoremax]] , aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=df_cores$species_guild_b[[kcoremax]], fill = "red"), size=4)


pointer_x <- max(topxa, topxb)+hop_x
pointer_y <- ymax

#for kc (in kcoremax-1:2)

zig <-  draw_ziggurat(basex = pointer_x, widthx = 1.2, basey = -15, widthy = 4, colorb = "green", numboxes = 5, zinverse = "no", grafo = p)
p <- zig["p"][[1]]
dr3 <- zig["dr"][[1]]

zig <-  draw_ziggurat(basex = pointer_x, widthx = 1.2, basey = 15, widthy = 4, colorb = "blue", numboxes = 4, zinverse = "yes", grafo = p)
p <- zig["p"][[1]]
dr4 <- zig["dr"][[1]]


zig <-  draw_ziggurat(basex = 5, widthx = 1.2, basey = 0, widthy = 4, colorb = "gray70", numboxes = 4, zinverse = "no", grafo = p)
p <- zig["p"][[1]]
dr5 <- zig["dr"][[1]]

for (j in 1: num_a_coremax)
{
  for (i in 1:num_b_coremax)
  {
    if (runif(1,0,1)>0.5)
    {
      link <- data.frame(x1=c(list_dfs_a[[kcoremax]][j,]$x1 + (list_dfs_a[[kcoremax]][j,]$x2-list_dfs_a[[kcoremax]][j,]$x1)/2), x2 = c(list_dfs_b[[kcoremax]][i,]$x1 +(list_dfs_b[[kcoremax]][i,]$x2-list_dfs_b[[kcoremax]][i,]$x1)/2), y1 = c(list_dfs_a[[kcoremax]][j,]$y1),  y2 = c(list_dfs_b[[kcoremax]][i,]$y1) )
      rndcol <- sample(vcols,1)
      p <- p + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=1, color=rndcol,alpha=0.2)
    }
  }
}
# 
# for (j in 1: 5)
# {
#   for (i in 1:4)
#   {
#     if (runif(1,0,1)>0.6)
#     {
#     link <- data.frame(x1=c(dr1[j,]$x2), x2 = c(dr4[i,]$x1), y1 = c(dr1[j,]$y2),  y2 = c(dr4[i,]$y2) )
#     rndcol <- sample(vcols,1)
#     p <- p + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=1, color=rndcol,alpha=0.2)
#     }
#   }
# }
# 
# for (j in 1: 5)
# {
#   for (i in 1:4)
#   {
#     if (runif(1,0,1)>0.35)
#     {
#       link <- data.frame(x1=c(dr2[j,]$x2), x2 = c(dr3[i,]$x1), y1 = c(dr2[j,]$y2),  y2 = c(dr3[i,]$y2) )
#       p <- p + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=0.6, color="gray70",alpha=0.5)
#     }
#   }
# }
# 
# for (j in 1: 4)
# {
#   for (i in 1:5)
#   {
#     if (runif(1,0,1)>0.6)
#     {
#       link <- data.frame(x1=c(dr4[j,]$x1), x2 = c(dr3[i,]$x1), y1 = c(dr4[j,]$y2),  y2 = c(dr3[i,]$y2) )
#       p <- p + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=0.6, color="gray70",alpha=0.5)
#     }
#   }
# }
p <- p + coord_fixed(ratio = 0.2) + theme_bw() + theme(panel.grid.minor.x = element_blank(),
                            panel.grid.minor.y = element_blank(),
                            panel.grid.major.x = element_blank(),
                            panel.grid.major.y = element_blank())
print(p)