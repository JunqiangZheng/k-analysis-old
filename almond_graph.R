library(scales)
library(grid)
library(gridExtra)
source("network-kanalysis.R")

str_guild_a <- "pl"
str_guild_b <- "pol"
directorystr <- "data/"
red <- "M_PL_058.csv"
result_analysis <- analyze_network(red, directory = directorystr, guild_a = str_guild_a, guild_b = str_guild_b, plot_graphs = TRUE)

nodes_guild_a <- grep(str_guild_a,V(result_analysis$graph)$name)
nodes_guild_b <- grep(str_guild_b,V(result_analysis$graph)$name)
nw_guild_a <-  V(result_analysis$graph)[nodes_guild_a]
nw_guild_b <-  V(result_analysis$graph)[nodes_guild_b]

draw_coremax_triangle <- function(basex,topx,basey,topy,numboxes,fillcolor)
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  xstep <- (topx-basex)*1/numboxes
  ystep <- (topy-basey)*1/numboxes
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


draw_ziggurat <- function(basex,topx,basey,topy,numboxes,fillcolor,inverse = "no")
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  
  xstep <- (topx-basex)*1/numboxes
  ystep <- (topy-basey)*1/numboxes
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
    d1 <-  d1[rev(rownames(d1)),]
    #names(d1) <- c("x1","x2","y1","y2","r")
  }
  
  return(d1)
}

basex <- 0
basey <- 0.8
topx <- 1
topy <- 5+basey
numboxes <- 5

dr1 <- draw_coremax_triangle(basex,topx,basey,topy,numboxes,"red")
p <- ggplot() +
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=dr1, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "red", color="red", alpha=0.5) +
  geom_text(data=dr1, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r, fill = "red"), size=4)

basex <- 0
basey <- -0.8
topx <- 1
topy <- -5+basey
numboxes <- 5
dr2 <- draw_coremax_triangle(basex,topx,basey,topy,numboxes,"blue")

p <- p + geom_rect(data=dr2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "blue", color="blue", alpha=0.5) +
     geom_text(data=dr2, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r, fill = "red"), size=4)



basex <- 2
basey <- -20
topx <- basex +1
topy <- basey +4
numboxes <- 5
dr3 <- draw_ziggurat(basex,topx,basey,topy,numboxes,"red")
p <- p + geom_rect(data=dr3, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "red", color="red", alpha=0.5) +
  geom_text(data=dr3, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r, fill = "red"), size=4)


basex <- 2
basey <- 20
topx <- basex +1
topy <- basey - 4
numboxes <- 4
dr4 <- draw_ziggurat(basex,topx,basey,topy,numboxes,"red", inverse ="yes")
p <- p + geom_rect(data=dr4, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "blue", color="blue", alpha=0.5) +
  geom_text(data=dr4, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r, fill = "red"), size=4)

for (j in 1: 5)
{
  for (i in 1:4)
  {
    if (runif(1,0,1)>0.2)
    {
      link <- data.frame(x1=c(dr1[j,]$x1 + (dr1[j,]$x2-dr1[j,]$x1)/2), x2 = c(dr2[i,]$x2 +(dr2[j,]$x2-dr2[j,]$x1)/2), y1 = c(dr1[j,]$y1),  y2 = c(dr2[i,]$y1) )
      p <- p + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=0.6, color="green",alpha=0.5)
    }
  }
}

for (j in 1: 5)
{
  for (i in 1:4)
  {
    if (runif(1,0,1)>0.4)
    {
    link <- data.frame(x1=c(dr1[j,]$x2), x2 = c(dr4[i,]$x1), y1 = c(dr1[j,]$y2),  y2 = c(dr4[i,]$y2) )
    p <- p + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=0.6, color="green",alpha=0.5)
    }
  }
}

for (j in 1: 5)
{
  for (i in 1:4)
  {
    if (runif(1,0,1)>0.35)
    {
      link <- data.frame(x1=c(dr2[j,]$x2), x2 = c(dr3[i,]$x1), y1 = c(dr2[j,]$y2),  y2 = c(dr3[i,]$y2) )
      p <- p + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=0.6, color="green",alpha=0.5)
    }
  }
}

for (j in 1: 4)
{
  for (i in 1:5)
  {
    if (runif(1,0,1)>0.65)
    {
      link <- data.frame(x1=c(dr4[j,]$x1), x2 = c(dr3[i,]$x1), y1 = c(dr4[j,]$y2),  y2 = c(dr3[i,]$y2) )
      p <- p + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=0.6, color="green",alpha=0.5)
    }
  }
}
p <- p + theme_bw() + theme(panel.grid.minor.x = element_blank(),
                            panel.grid.minor.y = element_blank())
print(p)

# d2 <- data.frame(x1, x2, -y2, -y1, rdr1+numboxes)
# names(d2) <- c("x1","x2","y1","y2","r")
# p <- ggplot() + 
#   scale_x_continuous(name="x") + 
#   scale_y_continuous(name="y") +
#   geom_rect(data=d1, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "red", color="white", alpha=0.5) +
#   geom_text(data=d1, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r, fill = "red"), size=4) +
#   geom_rect(data=d2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), , fill="blue", color="white", alpha=0.5) +
#   geom_text(data=d2, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4)
# 
# x1 <- c()
# x2 <- c()
# y1 <- c()
# y2 <- c()
# r <- c()
# basex <- 3
# basey <- 5
# topx <- 4
# topy <- 6
# xstep <- (topx-basex)*1/numboxes
# ystep <- (topy-basey)*1/numboxes
# for (j in (1:numboxes))
# {
#   x1 <- c(x1, basex+(j-1)*xstep)
#   x2 <- c(x2, x1[j]+0.9*xstep)
#   y1 <- c(y1, basey)
#   y2 <- c(y2, topy-(j-1)*ystep)
#   r <- c(r,j)
# }
# d3 <- data.frame(x1, x2, y1, y2, r)
# p <- p + geom_rect(data=d3, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = "red", color="white", alpha=0.5) +
#      geom_text(data=d1, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r, fill = "red"), size=4) 
# 
# print(p)