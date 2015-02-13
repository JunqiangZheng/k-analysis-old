library(scales)
library(grid)
library(gridExtra)
source("network-kanalysis.R")


gen_sq_label <- function(nodes)
{
  nnodes <- length(nodes)
  lrow <- round(sqrt(nnodes))
  ssal <- ""
  for (i in 1:nnodes)
  {
    ssal <- paste(ssal,nodes[i])
    if ((i %% lrow == 0) & (nnodes > 1))
      ssal <- paste(ssal,"\n")
  }
  return(ssal)
}

draw_square<- function(grafo,basex,basey,side,fillcolor,slabel,aspect_ratio,inverse="no")
{
  x1 <- c(basex)
  x2 <- c(basex+side)
  y1 <- c(basey)
  y2 <- c(basey+side/aspect_ratio)
  ds <- data.frame(x1, x2, y1, y2, fillcolor)
  signo <- 1
  if (inverse == "yes")
  {
    ds$y1 <- -(ds$y1)
    ds$y2 <- -(ds$y2)
    signo <- -1
  }  
  p <- grafo + geom_rect(data=ds, 
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
              fill = fillcolor, alpha = alpha_level,color="transparent")
  p <- p +annotate(geom="text", x=x1+0.05*(x2-x1), y=signo*(y1+(y2-y1)/2), label=slabel, 
              colour = fillcolor, size=labels_size, hjust = 0,  guide =FALSE)
  
  return(p)
}

draw_rectangle<- function(basex,basey,widthx,widthy,grafo,fillcolor,slabel,inverse="no")
{
  x1 <- c(basex)
  x2 <- c(basex+widthx)
  y1 <- c(basey)
  y2 <- c(basey+widthy)
  ds <- data.frame(x1, x2, y1, y2, fillcolor)
  signo <- 1
  if (inverse == "yes")
  {
    ds$y1 <- -(ds$y1)
    ds$y2 <- -(ds$y2)
    signo <- -1
  }  
  p <- grafo + geom_rect(data=ds, 
                         mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                         fill = fillcolor, alpha = alpha_level,color="transparent")
  p <- p +annotate(geom="text", x=x1+(x2-x1)/4, y=signo*(y1+(y2-y1)/2), label=slabel, 
                   colour = fillcolor, size=labels_size, hjust = 0,  guide =FALSE)
  
  return(p)
}


draw_tail <- function(p,fat_tail,ymax,lado,color,sqlabel,aspect_ratio,basex,basey,gap,
                      pintalinks,lxx2=0,lyy2=0,sqinverse = "no", position = "West")
{
  sidex <- min(0.25*ymax,1.5*lado*sqrt(nrow(fat_tail)))
  gap <- max(sidex*1.5,4*lado)
  signo <- 1
  if (sqinverse=="yes")
    signo <- -1
  if (position == "West"){
    xx <- basex-gap
    posxx1 <- xx+sidex
  }
  else if (position == "East"){
    xx <- basex+gap
    posxx1 <- xx
  }
  yy <- abs(basey)
  p <- draw_square(p,xx,yy,sidex,color,slabel=sqlabel,aspect_ratio,inverse = sqinverse)
  if (pintalinks){
    p <- draw_link(p, xx1=posxx1, xx2 = lxx2, 
                   yy1 = signo*(yy+sidex/(2*aspect_ratio)), yy2 = lyy2+signo*sidex/(2*aspect_ratio),
                   slink = size_link, clink = color_link, alpha_l = alpha_link )
  }
  calc_vals <- list("p" = p, "sidex" = sidex) 
  return(calc_vals)
}

draw_coremax_triangle <- function(basex,topx,basey,topy,numboxes,fillcolor,strlabels,
                                  igraphnet,strguild)
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  col_row <- c()
  xstep <- (topx-basex)*1/numboxes
  ystep <- (topy-basey)*0.8/numboxes
  for (j in (1:numboxes))
  {
    x1 <- c(x1, basex+(j-1)*xstep)
    x2 <- c(x2, x1[j]+0.9*xstep)
    y1 <- c(y1, basey)
    y2 <- c(y2, topy-(j-1)*ystep)
    r <- c(r,j)
    col_row <- c(col_row,fillcolor[1+j%%2])
  }
  d1 <- data.frame(x1, x2, y1, y2, r, col_row)
  d1$kdegree <- 0
  d1$label <- strlabels
  for (i in 1:nrow(d1))
    d1[i,]$kdegree <- igraphnet[paste0(strguild,d1[i,]$label)]$kdegree
  d1[rev(order(d1$kdegree)),]
  return(d1)
}

conf_ziggurat <- function(basex,widthx,basey,ystep,numboxes,fillcolor, strlabels, inverse = "no", edge_tr = "no")
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  col_row <- c()
  if (edge_tr == "no"){
    xstep <- widthx/numboxes
    if (numboxes > 3)
    {
      fmult_hght <- 1
      topx <- basex + widthx
    }
    else{
      fmult_hght <- 1.8
      basex <- basex + 0.2*widthx
      topx <- basex + 0.8*widthx
    }
  }
  else{
    if (numboxes > 8){
      fmult_hght <- 2
    }
    xstep <- 1.2*widthx/numboxes
    jump <- 0.25*min((1+0.05*numboxes),2)
    basex <- basex + jump*widthx
    topx <- basex + 0.9*widthx
    fmult_hght <- 1.4
  }
  for (j in (1:numboxes))
  {
    x1 <- c(x1, basex+(j-1)*xstep/2)
    if (edge_tr == "yes")
      x2 <- c(x2, topx)
    else
      x2 <- c(x2, topx-(j-1)*xstep/2)
    y1 <- c(y1, basey-(j-1)*ystep*fmult_hght)
    y2 <- c(y2, y1[j]+ystep*0.9*fmult_hght)
    r <- c(r,j)
    col_row <- c(col_row,fillcolor[1+j%%2])
  }
  d1 <- data.frame(x1, x2, y1, y2, r, col_row)
  if (inverse == "yes")
  {
    d1$y1 <- -(d1$y1)
    d1$y2 <- -(d1$y2)
  }  
  d1$label <- strlabels
  return(d1)
}

draw_ziggurat <- function(basex = 0, widthx = 0, basey = 0, ystep = 0, numboxes = 0, strlabels = "", sizelabels = 3, colorb = "", grafo = "", zinverse ="no", edge = "no")
{
  dr <- conf_ziggurat(basex,widthx,basey,ystep,numboxes,colorb, strlabels, inverse = zinverse, edge_tr = edge)
  print(dr$alpha_row)
  p <- grafo + geom_rect(data=dr, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = dr$col_row, alpha = alpha_level,color="transparent") +
    geom_text(data=dr, aes(x=x1+(x2-x1)/2, y= y1+(y2-y1)/2), color=dr$col_row, label = strlabels, size=sizelabels)
  calc_grafs <- list("p" = p, "dr" = dr) 
  return(calc_grafs)
}


find_orphans <- function(mtxlinks,orphans,gnet,guild_a="yes")
{
  m <- 0
  orph <- NA
  partner <- NA
  kcore <- NA
  df_orph <- data.frame(orph,partner,kcore)
  for (i in orphans)
  {
    if (guild_a == "yes")
    {
      partner <- mtxlinks[(mtxlinks$guild_a == paste0(str_guild_a,i)),]$guild_b
      str_opp <- str_guild_b
      str_own <- str_guild_a
    }
    else{
      partner <- mtxlinks[(mtxlinks$guild_b == paste0(str_guild_b,i)),]$guild_a
      str_opp <- str_guild_a
      str_own <- str_guild_b
    }
    for (t in 1:length(partner))
    {
      print(paste(str_own,i," partner ", partner[t],g[as.character(partner[t])]$kcorenum))
      m <- m+1
      df_orph[m,]$orph <- i
      df_orph[m,]$partner <- strsplit(as.character(partner[t]),str_opp)[[1]][2]
      df_orph[m,]$kcore <- g[as.character(partner[t])]$kcorenum
      print(paste(str_own,df_orph[m,]$orph," partner ", df_orph[m,]$partner,df_orph[m,]$kcore ))
    }
  }
  df_orph <- df_orph[!is.na(df_orph$orph),]
  return(df_orph)
}

draw_link <- function(grafo, xx1 = 0,xx2 = 0,yy1 = 0,yy2 = 0,slink = 1,clink = "gray70",alpha_l = 0.1)
{
  link <- data.frame(x1=c(xx1), x2 = c(xx2), y1 = c(yy1),  y2 = c(yy2))
  p <- grafo + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=slink, color=clink ,alpha=alpha_l)
  return(p)
}

str_guild_a <- "pl"
str_guild_b <- "pol"
directorystr <- "data/"
aspect_ratio <-1 
pintalinks <- TRUE
color_link <- "gray80"
alpha_link <- 0.2
size_link <- 1
labels_size <- 3
lsizetails <- 3
# displace_y_a <- c(0,0,0,0,0.05,0.05,-0.05,0)
# displace_y_b <- c(0,0.1,0,0.05,0,0,0,0)
displace_y_a <- c(0,0,0,0,0,0,0,0)
displace_y_b <- c(0,0,0,0,0,0,0,0)
red <- "M_PL_058.csv"
result_analysis <- analyze_network(red, directory = directorystr, guild_a = str_guild_a, guild_b = str_guild_b, plot_graphs = TRUE)
g <- V(result_analysis$graph)
g <- g[g$kdistance != Inf]
nodes_guild_a <- grep(str_guild_a,g$name)
nodes_guild_b <- grep(str_guild_b,g$name)
nw_guild_a <-  g[nodes_guild_a]
nw_guild_b <-  g[nodes_guild_b]
ind_cores <- rev(sort(unique(g$kcorenum)))
kcoremax <- max(ind_cores)
species_guild_a <- rep(NA,kcoremax)
species_guild_b <- rep(NA,kcoremax)
num_species_guild_a <- rep(NA,kcoremax)
num_species_guild_b <- rep(NA,kcoremax)
df_cores <- data.frame(species_guild_a, species_guild_b, num_species_guild_a, num_species_guild_b)

list_dfs_a <- list()
list_dfs_b <- list()
alpha_level <- 0.2
for (i in ind_cores) {
  df_cores[i,]$species_guild_a <- list(which(g[grep(str_guild_a,g$name)]$kcorenum == i))
  df_cores[i,]$num_species_guild_a <- length((df_cores[i,]$species_guild_a)[[1]])
  df_cores[i,]$species_guild_b <- list(which(g[grep(str_guild_b,g$name)]$kcorenum == i))
  df_cores[i,]$num_species_guild_b <- length((df_cores[i,]$species_guild_b)[[1]])
}

max_species_cores <- max(df_cores[kcoremax,]$num_species_guild_a,df_cores[kcoremax,]$num_species_guild_b)


color_guild_a <- c("#4169E1","#00008B")
color_guild_b <- c("#F08080","#FF0000")
scaling_factor <- length(g)%/%100

num_a_coremax <- df_cores[kcoremax,]$num_species_guild_a
basex <- -(8+aspect_ratio) * num_a_coremax

tot_width <- ((kcoremax >3 ) + 1) * 350
hop_x <- 0.85*(tot_width)/(kcoremax-2)
topxa <- min(0.75*hop_x,(0.5+(1/aspect_ratio))*hop_x)
height_y <- (0.8+0.1*kcoremax)*((0.8+max(0.1,1/aspect_ratio))*4+length(g)%/%100)
yoffset <- max(df_cores[2,]$num_species_guild_b, df_cores[2,]$num_species_guild_a)*height_y*1.3
ymax <- min(2*kcoremax*hop_x, max(15*height_y,0.9*yoffset)+height_y*length(g)/3)
miny <- ymax
maxy <- ymax
basey <- 0.05*ymax
topy <- 0.15*ymax+basey

strips_height <- 0.5*ymax/(kcoremax-2)

list_dfs_a[[kcoremax]] <- draw_coremax_triangle(basex,topxa,basey,topy,
                                                num_a_coremax,color_guild_a,
                                                df_cores$species_guild_a[[kcoremax]],
                                                g, str_guild_a)
p <- ggplot() +
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=list_dfs_a[[kcoremax]], mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = list_dfs_a[[kcoremax]]$col_row,  color="transparent",alpha=alpha_level) +
  geom_text(data=list_dfs_a[[kcoremax]], aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=df_cores$species_guild_a[[kcoremax]]), color = list_dfs_a[[kcoremax]]$col_row, size=labels_size, alpha = 1)
num_b_coremax <- df_cores[kcoremax,]$num_species_guild_b
basey <- - basey
topxb <- topxa 
topy <- - topy

list_dfs_b[[kcoremax]] <- draw_coremax_triangle(basex,topxb,basey,topy,num_b_coremax,
                                                color_guild_b,
                                                df_cores$species_guild_b[[kcoremax]],
                                                g, str_guild_b)
p <- p + geom_rect(data=list_dfs_b[[kcoremax]] , mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = list_dfs_b[[kcoremax]]$col_row,  color="transparent", alpha=alpha_level) +
      geom_text(data=list_dfs_b[[kcoremax]] , aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=df_cores$species_guild_b[[kcoremax]]), color = list_dfs_b[[kcoremax]]$col_row, size=labels_size)
  
pointer_x <- max(topxa, topxb)+hop_x
pointer_y <- ymax+abs(basey)
width_zig <- 0.4*hop_x

for (kc in seq(kcoremax-1,2))
{
  pointer_x <- (kcoremax-kc)*hop_x
  pointer_y <- pointer_y - (0.8+0.1*(kcoremax-kc)/kcoremax)*strips_height
  if (df_cores[kc,]$num_species_guild_a>0){
    despl_pointer_y <- displace_y_a[kc]*ymax
    if ((kc == 2) & (kcoremax >2))
    {
      if (df_cores[kc,]$num_species_guild_a>10)
        pointer_y <- yoffset
      else
        pointer_y <- yoffset+2*abs(basey)
      edge_core <- "yes"
      }
    else {
      edge_core <- "no"
      if (df_cores[kc-1,]$num_species_guild_a>5){
        pointer_y <- pointer_y - 0.2*height_y*df_cores[kc,]$num_species_guild_a
        pointer_x <- 1.1*pointer_x
        }
    }
    zig <-  draw_ziggurat(basex = pointer_x, widthx = (1+(kc/kcoremax))*width_zig, 
                          basey = pointer_y + despl_pointer_y, ystep = height_y, strlabels = df_cores$species_guild_a[[kc]], 
                          sizelabels = labels_size, colorb = color_guild_a, numboxes = df_cores[kc,]$num_species_guild_a, 
                          zinverse = "yes", edge = edge_core, grafo = p)
    p <- zig["p"][[1]]
    list_dfs_a[[kc]] <- zig["dr"][[1]]
    maxy <- -max(list_dfs_a[[kc]]$y2)
  }
  if (df_cores[kc,]$num_species_guild_b>0){
    despl_pointer_y <- displace_y_b[kc] * ymax
    if ((kc == 2) & (kcoremax >2))
    {
      if (df_cores[kc,]$num_species_guild_b>10)
        pointer_y <- yoffset
      else
        pointer_y <- yoffset+2*abs(basey)
      edge_core <- "yes"
      }
    else {
      edge_core <- "no"
      if (df_cores[kc-1,]$num_species_guild_b>5){
        pointer_y <- pointer_y - 0.2*height_y*df_cores[kc,]$num_species_guild_b
        pointer_x <- 1.1*pointer_x
      }
    }
    zig <-  draw_ziggurat(basex = pointer_x, widthx = (1+(kc/kcoremax))* width_zig, 
                          basey = pointer_y + despl_pointer_y,  ystep = height_y, strlabels = df_cores$species_guild_b[[kc]], 
                          colorb = color_guild_b, numboxes = df_cores[kc,]$num_species_guild_b, 
                          zinverse = "no", edge = edge_core, grafo = p)
    p <- zig["p"][[1]]
    list_dfs_b[[kc]]<- zig["dr"][[1]]
    miny <- min(list_dfs_b[[kc]]$y2)
  }
}

mtxlinks <- data.frame(get.edgelist(result_analysis$graph))
names(mtxlinks) <- c("guild_a","guild_b")
if (length(grep(str_guild_b,mtxlinks[1,1]))>0)
  names(mtxlinks) <- rev(names(mtxlinks))

orphans_a <- df_cores$species_guild_a[[1]]
orphans_b <- df_cores$species_guild_b[[1]]

df_orph_a <- find_orphans(mtxlinks,orphans_a,g,guild_a="yes")
max_b_kdegree <- list_dfs_b[[kcoremax]][which(list_dfs_b[[kcoremax]]$kdegree == max(list_dfs_b[[kcoremax]]$kdegree)),]$label
fat_tail_a <- df_orph_a[df_orph_a$partner == max_b_kdegree,]


df_orph_b <- find_orphans(mtxlinks,orphans_b,g,guild_a="no")
max_a_kdegree <- list_dfs_a[[kcoremax]][which(list_dfs_a[[kcoremax]]$kdegree == max(list_dfs_a[[kcoremax]]$kdegree)),]$label
fat_tail_b <- df_orph_b[df_orph_b$partner == max_a_kdegree,]

lado <- 0.1* max (abs(list_dfs_b[[kcoremax]][1,]$y1-list_dfs_b[[kcoremax]][1,]$y2)/aspect_ratio,
             abs(list_dfs_a[[kcoremax]][1,]$y1-list_dfs_a[[kcoremax]][1,]$y2)/aspect_ratio)
lado <- height_y
gap <-  min(0.4*tot_width,5*lado*(1+1/(aspect_ratio)^1/4))

if (nrow(fat_tail_a)>0){
  v<- draw_tail(p,fat_tail_a,ymax,lado,color_guild_a[1],gen_sq_label(fat_tail_a$orph),
                aspect_ratio,list_dfs_b[[kcoremax]][1,]$x1,basey,gap,pintalinks,lxx2 = list_dfs_b[[kcoremax]][1,]$x1,
                lyy2 = list_dfs_b[[kcoremax]][1,]$y1,sqinverse = "yes")
  p <- v["p"][[1]]
  }
if (nrow(fat_tail_b)>0){
  v<- draw_tail(p,fat_tail_b,ymax,lado,color_guild_b[1],gen_sq_label(fat_tail_b$orph),
                aspect_ratio,basex,basey,gap,pintalinks,
                lxx2 = list_dfs_a[[kcoremax]][1,]$x1, lyy2 = list_dfs_a[[kcoremax]][1,]$y1,
                sqinverse = "no")
  p <- v["p"][[1]]
  }
if (kcoremax >2)
{
  long_tail_a <- df_orph_a[df_orph_a$kcore == 2,]
  point_x <- list_dfs_a[[2]][nrow(list_dfs_a[[2]]),]$x2
  point_y <- list_dfs_a[[2]][nrow(list_dfs_a[[2]]),]$y1
  long_tail_b <- df_orph_b[df_orph_b$kcore == 2,]
  if (length(long_tail_b)>0)
  {
    signo <- -1
    list_spec_a <- rev(list_dfs_a[[2]]$label)
    for (i in list_spec_a)
    {
      conn_species <- which(long_tail_b$partner == i)
      if (length(conn_species)>0)
        {
          little_tail <- long_tail_b[long_tail_b$partner == i,]
          v<- draw_tail(p,little_tail,ymax,lado,color_guild_b[1],gen_sq_label(little_tail$orph),
                        aspect_ratio,point_x,point_y,gap,pintalinks,lxx2 = list_dfs_a[[2]][which(list_dfs_a[[2]]$label == i),]$x2,
                        lyy2 = list_dfs_a[[2]][which(list_dfs_a[[2]]$label == i),]$y1,sqinverse = "yes", position = "East")
          p <- v["p"][[1]]
          salto <- v["sidex"][[1]]/aspect_ratio
          point_y <- point_y + 1.2*signo*salto
        }
    }
  }
}


if (pintalinks)
{
  for (kcb in seq(kcoremax,2))
  {
    for (kc in seq(kcoremax,2))
    {
      for (j in 1:length(list_dfs_a[[kc]]$label))
      {
          for (i in 1:length(list_dfs_b[[kcb]]$label))
          {
            if (sum(mtxlinks$guild_a == paste0(str_guild_a,list_dfs_a[[kc]][j,]$label) & mtxlinks$guild_b == paste0(str_guild_b,list_dfs_b[[kcb]][i,]$label))>0)
            {
              if ((kc == kcoremax) & (kcb == kcoremax))
              {
              link <- data.frame(x1=c(list_dfs_a[[kc]][j,]$x1 + (list_dfs_a[[kc]][j,]$x2-list_dfs_a[[kc]][j,]$x1)/2), 
                                 x2 = c(list_dfs_b[[kcb]][i,]$x1 +(list_dfs_b[[kcb]][i,]$x2-list_dfs_b[[kcb]][i,]$x1)/2), 
                                 y1 = c(list_dfs_a[[kc]][j,]$y1),  y2 = c(list_dfs_b[[kcb]][i,]$y1) )
              lcolor = "orange"
              }
              else if (kc == kcb) {
              link <- data.frame(x1=c(list_dfs_a[[kc]][j,]$x1), 
                                 x2 = c(list_dfs_b[[kcb]][i,]$x1), 
                                 y1 = c(list_dfs_a[[kc]][j,]$y1),  y2 = c(list_dfs_b[[kcb]][i,]$y1) )
              lcolor = "pink"
              }
              else if (kc > kcb) {
                if (kc == kcoremax)
                  link <- data.frame(x1=c(list_dfs_a[[kc]][j,]$x2), 
                                     x2 = c(list_dfs_b[[kcb]][i,]$x1), 
                                     y1 = c(list_dfs_a[[kc]][j,]$y2),  y2 = c(list_dfs_b[[kcb]][i,]$y1) )
                else 
                  link <- data.frame(x1=c(list_dfs_a[[kc]][j,]$x2), 
                                  x2 = c(list_dfs_b[[kcb]][i,]$x1), 
                                  y1 = c(list_dfs_a[[kc]][j,]$y1),  y2 = c(list_dfs_b[[kcb]][i,]$y1) )
              lcolor = "green"
              }
              else
              {
                if (kcb == kcoremax)
                  y_2 <- c(list_dfs_b[[kcb]][i,]$y2)
                else
                  y_2 <- c(list_dfs_b[[kcb]][i,]$y1)
                link <- data.frame(x1=c(list_dfs_a[[kc]][j,]$x1), 
                                   x2 = c(list_dfs_b[[kcb]][i,]$x2), 
                                   y1 = c(list_dfs_a[[kc]][j,]$y1),  y2 = y_2)
              lcolor = "blue" 
              }
              p <- p + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=1, color=lcolor,alpha=alpha_link)
            }
          }
      }      
    }
  }
}
p <- p + coord_fixed(ratio=aspect_ratio) +theme_bw() + theme(panel.grid.minor.x = element_blank(),
                            panel.grid.minor.y = element_blank(),
                            panel.grid.major.x = element_blank(),
                            panel.grid.major.y = element_blank())
print(p)