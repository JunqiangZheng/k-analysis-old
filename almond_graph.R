library(scales)
library(grid)
library(gridExtra)
library(Hmisc)
library(RColorBrewer)
source("network-kanalysis.R")


gen_sq_label <- function(nodes, joinchars = "\n")
{
  nnodes <- length(nodes)
  lrow <- round(sqrt(nnodes))
  ssal <- ""
  for (i in 1:nnodes)
  {
    ssal <- paste(ssal,nodes[i])
    if ((i %% lrow == 0) & (nnodes > 1))
      ssal <- paste(ssal,joinchars)
  }
  return(ssal)
}

draw_square<- function(grafo,basex,basey,side,fillcolor,alphasq,labelcolor,
                       langle,hjust,vjust,slabel,aspect_ratio,labels_size,
                       inverse="no",adjustoxy = "no", edgescolor="transparent")
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
                         fill = fillcolor, alpha = alphasq, color=edgescolor)
  pxx <- x1+0.05*(x2-x1)
  if (adjustoxy == "no")
    pyy <- signo*(y1+(y2-y1)/2)
  else
  {
    if (signo == 1)
      yjump <- 0.15*(y2-y1)
    else
      yjump <- -0.85*(y2-y1)
    pyy <- signo*(y1)+yjump
  }
  p <- p +annotate(geom="text", x=pxx, y=pyy, label=slabel, 
                   colour = labelcolor, size=labels_size, hjust = hjust, vjust = vjust, angle = langle,  
                   guide =FALSE)
  
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


draw_tail <- function(p,fat_tail,lado,color,sqlabel,aspect_ratio,basex,basey,gap,
                      pintalinks,lxx2=0,lyy2=0,sqinverse = "no", 
                      position = "West", background = "no", first_leaf = "yes", spline = "no")
{
  adjust <- "no"
  lhjust <- 0
  lvjust <- 0
  langle <- 0
  adjust <- "no"
  ecolor <- "transparent"
  bgcolor <- color
  labelcolor <- color
  palpha <- alpha_link
  sidex <- lado*sqrt(nrow(fat_tail))
  paintsidex <- sidex
  signo <- 1
  yy <- abs(basey)
  plxx2 <- lxx2
  plyy2 <- lyy2
  if (sqinverse=="yes")
    signo <- -1
  if (position == "West"){
    xx <- basex-gap
    posxx1 <- xx+sidex
    posyy1 = signo*(yy)+signo*(0.5*sidex/(aspect_ratio))
  }
  else if (position == "East"){
    gap <- hop_x/2
    xx <- basex+gap
    posxx1 <- xx
    posyy1 = signo*(yy+sidex/(2*aspect_ratio))
  }
  else if (position == "North"){
    xx <- basex-sidex/2
    posxx1 <- xx+sidex/2
    posyy1 = signo*(yy)
  }
  else if (position == "South"){
    xx <- basex-sidex/2
    posxx1 <- xx+sidex/2
    posyy1 = signo*(yy)
  }
  else if (position == "Floating_tail"){  
    xx <- basex-gap
    posxx1 <- xx
    posyy1 <- signo*(yy)#+0.5*signo*(sidex/(aspect_ratio))
    if (first_leaf == "no")
    {
      plyy2 <- posyy1-sidex/(aspect_ratio)
      print(paste("PLYY2", plyy2, "POSYY1", posyy1))
    }
  }
  if (background == "no")
  {
    ecolor <- "transparent"
    palpha <- alpha_level-0.12
    rot_angle <-20+kcoremax
    #rot_angle <- 0
    if (position == "North") 
    {
      langle <- rot_angle
      lvjust <- 0
    }
    if (position == "South") 
    {
      langle <- -rot_angle
      lvjust <- 0
    }
    if (position == "West"){
      adjust <- "yes"
      lvjust <- 0
      lhjust <- 0
    }
  }
  p <- draw_square(p,xx,yy,paintsidex,bgcolor,palpha,labelcolor,langle,lhjust,lvjust,
                   slabel=sqlabel,aspect_ratio,lsizetails,inverse = sqinverse, 
                   adjustoxy = adjust, edgescolor = ecolor)
  if (pintalinks){
    p <- draw_link(p, xx1=posxx1, xx2 = plxx2, 
                   yy1 = posyy1, yy2 = plyy2, 
                   slink = size_link, clink = sample(vcols_link,1), 
                   alpha_l = alpha_link , spline= spline)
  }
  calc_vals <- list("p" = p, "sidex" = sidex, "xx" = posxx1, "yy" = posyy1) 
  return(calc_vals)
}


draw_edge_tails <- function(p,kcoreother,long_tail,list_dfs,color_guild, inverse = "no", 
                            vertical = "yes", orientation = "East", revanddrop = "no", 
                            pbackground = "yes", joinchars = "\n", tspline = "no")
{
  if (orientation == "West")
    point_y <- point_y - gap
  rxx <- point_x
  ryy <- point_y

  joinstr <- joinchars
  signo <- 1
  if (inverse == "yes")
    signo <- -1
  list_spec <- list_dfs[[kcoreother]]$label
  if (revanddrop == "yes")
    list_spec <- rev(list_spec)[1:length(list_spec)-1]
  if (orientation == "East")
    list_spec <- rev(list_spec)
  llspec <- length(list_spec)
  m <- 0
  separacion <- 0.035*tot_width
  for (i in list_spec)
  {
    conn_species <- which(long_tail$partner == i)
    if (length(conn_species)>0)
    {
      little_tail <- long_tail[long_tail$partner == i,]
      if ((orientation == "East") | (orientation == "West"))
      {
        data_row <- list_dfs[[kcoreother]][which(list_dfs[[kcoreother]]$label == i),]
        xx2 <- data_row$x2
        yy2 <- (data_row$y1 + data_row$y2)/2
      }
      else# if (orientation = "North")
      {
        rxx <- point_x
        ryy <- point_y
        data_row <- list_dfs[[kcoreother]][which(list_dfs[[kcoreother]]$label == i),]
        xx2 <- (data_row$x2+data_row$x1)/2
        yy2 <- data_row$y2
      }
      v<- draw_tail(p,little_tail,lado,color_guild[1],
                    gen_sq_label(little_tail$orph,joinchars = joinstr),
                    aspect_ratio,point_x,point_y,gap,pintalinks,lxx2 = xx2,
                    lyy2 = yy2, sqinverse = inverse, position = orientation,
                    background = pbackground, spline = tspline)
      p <- v["p"][[1]]
      rxx <- v["xx"][[1]]
      ryy <- v["yy"][[1]]
      if (vertical == "yes"){
        salto <- v["sidex"][[1]]/aspect_ratio
        point_y <- point_y + 2.4*signo*salto
        rxx <- point_x
      }
      
      # tails connected to kcoremax except first species
      
      else{ 
        if (orientation == "West")
          salto <- 0
        else 
          salto <- 0.4*v["sidex"][[1]]/aspect_ratio
        point_x <- point_x - separacion - v["sidex"][[1]]
        point_y <- point_y - 1.4*signo*salto
        ryy <- point_y
        rxx <- point_x
      }
    }
    m <- m +1
  }
  calc_vals <- list("p" = p, "lastx" = rxx, "lasty" = ryy) 
  return(calc_vals)
  
}


draw_coremax_triangle <- function(basex,topx,basey,topy,numboxes,fillcolor,strlabels,
                                  igraphnet,strguild,orderby = "None")
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  kdegree <- c()
  kdistance <- c()
  col_row <- c()
#   if (numboxes > 10)
#     pbasex <- basex-(abs(topx-basex)/2) 
#   else
  pbasex <- basex - (numboxes %/%8) * abs(topx-basex)/3
  xstep <- (topx-pbasex)*1/numboxes
  
  ystep <- (topy-basey)*0.7/numboxes
  for (j in (1:numboxes))
  {
    x1 <- c(x1, pbasex+(j-1)*xstep)
    x2 <- c(x2, x1[j]+0.9*xstep)
    y1 <- c(y1, basey)
    y2 <- c(y2, topy-(j-1)*ystep)
    r <- c(r,j)
    col_row <- c(col_row,fillcolor[1+j%%2])
    kdegree <- c(kdegree,0)
    kdistance <- c(kdistance,1)
  }
  d1 <- data.frame(x1, x2, y1, y2, r, col_row, kdegree, kdistance)
 
  d1$label <- strlabels
  for (i in 1:nrow(d1)){
    d1[i,]$kdegree <- igraphnet[paste0(strguild,d1[i,]$label)]$kdegree
    d1[i,]$kdistance <- igraphnet[paste0(strguild,d1[i,]$label)]$kdistance
  }
  if (orderby == "kdistance"){
    ordvector <- order(d1$kdistance)
    d1$label <- d1[ordvector,]$label
    d1$kdistance <- d1[ordvector,]$kdistance
    d1$kdegree <- d1[ordvector,]$kdegree
  }
  else if (orderby == "kdegree"){
    ordvector <- rev(order(d1$kdegree))
    d1$label <- d1[ordvector,]$label
    d1$kdistance <- d1[ordvector,]$kdistance
    d1$kdegree <- d1[ordvector,]$kdegree
  }
  return(d1)
}

conf_ziggurat <- function(igraphnet, basex,widthx,basey,ystep,numboxes,fillcolor, strlabels, strguild, inverse = "no", edge_tr = "no")
{ 
  
  kdeg <- rep(0,length(strlabels))
  kdist <- rep(1,length(strlabels))
  d2 <- data.frame(strlabels,kdeg,kdist)
  names(d2) <- c("label","kdegree","kdistance")
  for (i in 1:nrow(d2)){
    d2[i,]$kdegree <- igraphnet[paste0(strguild,d2[i,]$label)]$kdegree
    d2[i,]$kdistance <- igraphnet[paste0(strguild,d2[i,]$label)]$kdistance
  }
  d2 <- d2[order(d2$kdistance),]
  
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  col_row <- c()
  fmult_hght <- 1
  if (edge_tr == "no"){
    xstep <- widthx/numboxes
    #fmult_hght <- 1.8
    if (numboxes > 3)
    {
      topx <- basex + widthx
    }
    else{
      
      basex <- basex + 0.2*widthx
      topx <- basex + 0.8*widthx
    }
  }
  else{
    xstep <- 1.2*widthx/numboxes
    jump <- 0.25*min((1+0.05*numboxes),2)
    basex <- basex + jump*widthx
    topx <- basex + 0.9*widthx
    #fmult_hght <- 1.8
  }
  for (j in (1:numboxes))
  {
    #x1 <- c(x1, basex+(j-1)*xstep/2)
    x1 <- c(x1, basex+(j-1)*xstep/2.5)
    if (edge_tr == "yes")
      x2 <- c(x2, topx-(j-1)*xstep/8)
    else
      #x2 <- c(x2, topx-(j-1)*xstep/2)
      x2 <- c(x2, topx-(j-1)*xstep/4)
    y1 <- c(y1, basey-(j-1)*ystep*fmult_hght)
    y2 <- c(y2, y1[j]+ystep*fmult_hght)
    r <- c(r,j)
    col_row <- c(col_row,fillcolor[1+j%%2])
  }
  label <- d2$label
  kdegree <- d2$kdegree
  kdistance <- d2$kdistance
  d1 <- data.frame(x1, x2, y1, y2, r, col_row, label, kdegree, kdistance)
  if (inverse == "yes")
  {
    d1$y1 <- -(d1$y1)
    d1$y2 <- -(d1$y2)
  }  

  return(d1)

}

draw_ziggurat <- function(igraphnet, basex = 0, widthx = 0, basey = 0, ystep = 0, numboxes = 0, strlabels = "", strguild = "",
                          sizelabels = 3, colorb = "", grafo = "", zinverse ="no", edge = "no")
{
  dr <- conf_ziggurat(igraphnet, basex,widthx,basey,ystep,numboxes,colorb, strlabels, strguild, inverse = zinverse, edge_tr = edge)
  p <- grafo + geom_rect(data=dr, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                        fill = dr$col_row, alpha = alpha_level,color="transparent") +
                        geom_text(data=dr, 
                                  aes(x=x1+(1+(1-2*(max(r)-r)%%2)*0.95*((r-max(r))/max(r)) )*(x2-x1)/2, 
                                      y= y1+(y2-y1)/2), color=dr$col_row, 
                        label = dr$label, size=sizelabels)
 
  calc_grafs <- list("p" = p, "dr" = dr) 
  return(calc_grafs)
}


find_orphans <- function(mtxlinks,orphans,gnet,guild_a="yes")
{
  m <- 0
  orph <- NA
  partner <- NA
  kcore <- NA
  repeated <- NA
  df_orph <- data.frame(orph,partner,kcore,repeated)
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
      m <- m+1
      df_orph[m,]$orph <- i
      df_orph[m,]$partner <- strsplit(as.character(partner[t]),str_opp)[[1]][2]
      df_orph[m,]$kcore <- g[as.character(partner[t])]$kcorenum
      if (length(partner)>1)
        df_orph[m,]$repeated = "yes"
      else
        df_orph[m,]$repeated <- "no"
    }
  }
  df_orph <- df_orph[!is.na(df_orph$orph),]
  return(df_orph)
}

draw_link <- function(grafo, xx1 = 0,xx2 = 0,yy1 = 0,yy2 = 0,
                      slink = 1,clink = "gray70",alpha_l = 0.1, spline = "no")
{
  link <- data.frame(x1=xx1, x2 = xx2, y1 = yy1,  y2 = yy2)
  if (spline == "no")
    p <- grafo + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=slink, color=clink ,alpha=alpha_l)
  else{
     if (spline == "horizontal"){
       x <- c(link$x1,link$x1+(link$x2-link$x1)*0.05,link$x1+(link$x2-link$x1)*0.75,link$x2)
       y <- c(link$y1,link$y1+(link$y2-link$y1)*0.1,link$y1+(link$y2-link$y1)*0.65,link$y2)
     }
     else if (spline == "vertical"){
       x <- c(link$x1,link$x1+(link$x2-link$x1)*0.85,link$x2)
       y <- c(link$y1,link$y1+(link$y2-link$y1)*0.9,link$y2)
     }
     else if (spline == "diagonal"){
       x <- c(link$x1,link$x1+(link$x2-link$x1)*0.5,link$x2)
       y <- c(link$y1,link$y1+(link$y2-link$y1)*0.55,link$y2)
     }
     else if (spline == "lshaped"){
       #x <- c(link$x1,link$x1+(link$x2-link$x1)*0.90,link$x1+(link$x2-link$x1)*0.95,link$x2)
       x <- c(link$x1,link$x1+(link$x2-link$x1)*0.80,link$x1+(link$x2-link$x1)*0.90,link$x2)
       y <- c(link$y1,link$y1+(link$y2-link$y1)*0.95,link$y1+(link$y2-link$y1)*0.99,link$y2)
     }
     else if (spline == "weirdhorizontal"){
       x <- c(link$x1,link$x1+(link$x2-link$x1)*0.40,link$x1+(link$x2-link$x1)*0.75,link$x2)
       y <- c(link$y1,link$y1+(link$y2-link$y1)*0.6,link$y1+(link$y2-link$y1)*0.70,link$y2)
     }
     else if (spline == "arc"){
       x <- c(link$x1,link$x1+(link$x2-link$x1)*0.30,link$x2)
       y <- c(link$y1,link$y1+(link$y2-link$y1)*0.50,link$y2)
     }
     xout <- seq(min(x),max(x),length.out = 1000)
     s1 <- spline(x,y,xout=xout,method='natural')
     ds1 <- as.data.frame(s1)
     p <- grafo + geom_line(data =ds1,aes(x,y), size=slink, color=clink ,alpha=alpha_l)
  }
  return(p)
}

weird_analysis <- function(weirds,opposite_weirds,species)
{
  ldf <- weirds[weirds$orph == species,]
  if (max(ldf$kcore)>1)
    return(ldf)   
}

store_weird_species <- function (row_orph, df_store, strguild, lado, gap)
{
  
  sidex <- lado
  index <- nrow(df_store)+1
  df_store[index,]$kcorepartner <- row_orph$kcore
  separation <- 0.1*tot_width
  tot_weirds <- nrow(original_weirds_a)+nrow(original_weirds_b)
  jumpfactor <- (4-min(3,(tot_weirds%/%10)))
  cgap <- lado+gap/(5-min(3,(tot_weirds%/%10)))
  
  if (row_orph$kcore > 1){
    df_store[index,]$guild <- as.character(strguild)
    df_store[index,]$orph <- row_orph$orph
    df_store[index,]$partner <- row_orph$partner
    if (strguild == str_guild_b){
        data_row <- list_dfs_a[[row_orph$kcore]][list_dfs_a[[row_orph$kcore]]$label==row_orph$partner,]
    }
    if (strguild == str_guild_a){
        data_row <- list_dfs_b[[row_orph$kcore]][list_dfs_b[[row_orph$kcore]]$label==row_orph$partner,]
    }
    if (row_orph$kcore == kcoremax){
      if (strguild == str_guild_b){
        edge_row <- list_dfs_a[[kcoremax]][1,]
        xbase <-  min(last_xtail_a[[kcoremax]],edge_row$x1 - 6*gap) - gap
      }
      else{
        edge_row <- list_dfs_b[[kcoremax]][1,]
        xbase <-  min(last_xtail_b[[kcoremax]],edge_row$x1 - 6*gap) - gap
      }
      df_store[index,]$x1 <- xbase - 2*gap
      df_store[index,]$y1 <- max(abs(edge_row$y2),abs(edge_row$y1)) + 8*cgap/(aspect_ratio)
      if (df_store[index,]$guild == str_guild_a){
         df_store[index,]$y1 = -abs(df_store[index,]$y1)
         df_store[index,]$y2 = -abs(df_store[index,]$y2)
      }
      
      repetitions <- sum((df_store$partner == row_orph$partner) & (df_store$guild == strguild))
      if (repetitions > 1){
        print(paste("repetitions ",repetitions))
        df_store[index,]$x1 <- df_store[index,]$x1 -  (repetitions-1) * sidex
        df_store[index,]$y1 <- df_store[index,]$y1 +  sign(df_store[index,]$y1)*(repetitions-1) * (3+as.integer(kcoremax>3)) * sidex/aspect_ratio
      }
      repetitions_root <- sum((df_store$kcorepartner == kcoremax) & (df_store$guild == strguild))
      if (repetitions_root > 1){
        print(paste("repetitions root",repetitions_root))
        df_store[index,]$x1 <- df_store[index,]$x1 +  3* (repetitions_root) * (kcoremax) * sidex
        df_store[index,]$y1 <- df_store[index,]$y1 +  sign(df_store[index,]$y1)*(1/jumpfactor)*((repetitions_root+ 0.7*index) * 6* sidex/aspect_ratio)
      }

    }
    else{      
      if (strguild == str_guild_b)
      {
        data_row_pic <- list_dfs_a[[row_orph$kcore]]#[which(list_dfs_a[[row_orph$kcore]]$label == df_store[index,]$partner),]
        df_store[index,]$x1 <- min(data_row_pic$x2) + 3* separation
        df_store[index,]$y1 <- last_ytail_a[row_orph$kcore] - (4+sqrt(kcoremax))*sidex/aspect_ratio
        last_ytail_a[row_orph$kcore] <- -abs(df_store[index,]$y1)
      }
      if (strguild == str_guild_a)
      {
        data_row_pic <- list_dfs_b[[row_orph$kcore]]
        df_store[index,]$x1 <- min(data_row_pic$x2) + 3*separation
        df_store[index,]$y1 <- last_ytail_b[row_orph$kcore] +  (4+sqrt(kcoremax))*sidex/aspect_ratio
        last_ytail_b[row_orph$kcore] <- abs(df_store[index,]$y1)
      }
    }
  }
  else{
    print("aqui se tratan los de core 1")
    df_store[index,]$partner <- row_orph$orph
    df_store[index,]$orph <- row_orph$partner
    df_store[index,]$guild <- as.character(strguild)
    data_row <- df_store[(df_store$orph == row_orph$orph) & (swap_strguild(strguild) == df_store$guild),]
    repetitions <- sum((df_store$partner == row_orph$orph) & (df_store$guild == strguild))
    if (data_row$kcorepartner == kcoremax){
      df_store[index,]$x1 <- data_row$x1 - separation
      df_store[index,]$y1 <- data_row$y1 + sign(data_row$y1)*4*(repetitions-1)*sidex/aspect_ratio
    }
    else{
      df_store[index,]$x1 <- data_row$x1 + sign(data_row$x1)*separation
      df_store[index,]$y1 <- data_row$y1 + sign(data_row$y1)*4*(repetitions-1)*sidex/aspect_ratio
    }
    
  }
  df_store[index,]$x2 <- df_store[index,]$x1 + sidex
  df_store[index,]$y2 <- df_store[index,]$y1 + sidex/aspect_ratio
  if (row_orph$kcore == kcoremax){
    df_store[index,]$xx2 <- (data_row$x2+data_row$x1)/2
    df_store[index,]$yy2 <- data_row$y2
  }
  else
  {
    if (df_store[index,]$kcorepartner > 1){
      df_store[index,]$xx2 <- data_row$x2
      df_store[index,]$yy2 <- (data_row$y2+data_row$y1)/2
    }
    else {
      df_store[index,]$xx2 <- data_row$x1
      df_store[index,]$yy2 <- data_row$y1
    }
  }

  return(df_store)
}

draw_weird_chains <- function(grafo, df_chains, paintsidex, pintalinks)
{
  p <- grafo
  for (i in 1:nrow(df_chains))
  {
    sqinverse = "no"
    if (df_chains[i,]$guild == str_guild_a){
      bgcolor <- color_guild_a[1]  
    }
    if (df_chains[i,]$guild == str_guild_b)
      bgcolor <- color_guild_b[1]
    
    if (df_chains[i,]$kcorepartner == 1){
      hjust <- 0.5
      vjust <- 0
    }
    else{
      hjust <- 0
      vjust <- 0
    }
      
    p <- draw_square(p,df_chains[i,]$x1,df_chains[i,]$y1,paintsidex,bgcolor,alpha_level,
                     bgcolor,0,hjust,vjust,
                     slabel=df_chains[i,]$orph,aspect_ratio,
                     lsizetails,inverse = "no")
    if (pintalinks){
      
      xx2 = df_chains[i,]$xx2
      if (df_chains[i,]$kcorepartner == kcoremax){
        xx1 = df_chains[i,]$x2
        yy1 = df_chains[i,]$y1-sign(df_chains[i,]$y1)*(df_chains[i,]$y2-df_chains[i,]$y1)/2
      }
      else{
        xx1 = df_chains[i,]$x1
        yy1 = df_chains[i,]$y1
        if (df_chains[i,]$kcorepartner == 2)
          yy1 <- yy1 + paintsidex/(2*aspect_ratio)
      }
      yy2 = df_chains[i,]$yy2

      if (df_chains[i,]$kcorepartner == 1){
        yy1 = yy1 + 0.5*paintsidex/aspect_ratio
        yy2 = yy2 + 0.5*paintsidex/aspect_ratio
        if (xx2>0)
          xx2 <- xx2 + paintsidex
        else
          xx1 <- xx1 + paintsidex
      }
      if ( (df_chains[i,]$kcorepartner >1) & (df_chains[i,]$kcorepartner < kcoremax) )
      {
        splineshape = "lshaped"

      }
      else
        splineshape = "weirdhorizontal"
      
      #color_link = "green"
      
      p <- draw_link(p, xx1=xx1, xx2 = xx2, 
                     yy1 = yy1, yy2 = yy2, 
                     slink = size_link, clink = color_link, 
                     alpha_l = alpha_link , spline = splineshape)
    }
  }
  return(p)
}

swap_strguild <- function(strguild)
{
  if (strguild == str_guild_a)
    strguild = str_guild_b
  else
    strguild = str_guild_a
  return(strguild)
}

store_root_leaf <- function(weirds,df_chains,strguild, lado, gap)
{
  for (i in 1:nrow(weirds))
  {
    if (weirds[i,]$kcore > 1){
print(weirds[i,])
      df_chains <- store_weird_species(weirds[i,], df_chains, strguild, lado, gap)
print(df_chains)
      weirds[i,]$drawn = "yes"
    }
  }
  calc_vals <- list("df_chains" = df_chains, "weirds" = weirds) 
  return(calc_vals)
}

store_branch_leaf <- function(weirds, weirds_opp,df_chains, pstrguild, lado, gap)
{
  for (i in 1:nrow(weirds))
  {
    if (weirds[i,]$drawn == "no"){
      strguild <- pstrguild
      if (sum( ((df_chains$orph == weirds[i,]$orph) & (df_chains$guild == strguild)) )>0 ){
        strguild <- swap_strguild(pstrguild)
        df_chains <- store_weird_species(weirds[i,], df_chains, strguild, lado, gap)
        weirds[i,]$drawn = "yes"
        mirror_weird <- which((weirds_opp$partner == weirds[i,]$orph) & (weirds_opp$orph == weirds[i,]$partner))
        if (length(mirror_weird)>0)
          weirds_opp[mirror_weird , ]$drawn = "yes"
      }
    }
  }
  calc_vals <- list("df_chains" = df_chains, "weirds" = weirds, "weirds_opp" = weirds_opp) 
  return(calc_vals)
}


str_guild_a <- "pl"
str_guild_b <- "pol"
directorystr <- "data/"
pintalinks <- TRUE
color_link <- "gray80"
alpha_link <- 0.2
size_link <- 0.5

# displace_y_a <- c(0,0,0,0,0.05,0.05,-0.05,0)
# displace_y_b <- c(0,0.1,0,0.05,0,0,0,0)
displace_y_a <- c(0,0,0,0,0,0,0,0)
displace_y_b <- c(0,0,0,0,0,0,0,0)
aspect_ratio <- 1.2
print_to_file <- FALSE
labels_size <- 3 - as.integer(print_to_file)
lsizetails <- labels_size - 0.3
red <- "M_PL_053.csv"
network_name <- strsplit(red,".csv")[[1]][1]
joinstr <- " "
result_analysis <- analyze_network(red, directory = directorystr, guild_a = str_guild_a, 
                                   guild_b = str_guild_b, plot_graphs = TRUE)
g <- V(result_analysis$graph)
g <- g[g$kdistance != Inf]
nodes_guild_a <- grep(str_guild_a,g$name)
nodes_guild_b <- grep(str_guild_b,g$name)
ind_cores <- rev(sort(unique(g$kcorenum)))
kcoremax <- max(ind_cores)
species_guild_a <- rep(NA,kcoremax)
species_guild_b <- rep(NA,kcoremax)
num_species_guild_a <- rep(NA,kcoremax)
num_species_guild_b <- rep(NA,kcoremax)
last_xtail_a <- rep(NA,kcoremax)
last_ytail_a <- rep(NA,kcoremax)
last_xtail_b <- rep(NA,kcoremax)
last_ytail_b <- rep(NA,kcoremax)
df_cores <- data.frame(species_guild_a, species_guild_b, num_species_guild_a, num_species_guild_b)
list_dfs_a <- list()
list_dfs_b <- list()
alpha_level <- 0.2
mindist <- 1
maxdist <- 5^(1/4)
pal_b <-colorRampPalette(c("red","pink"))
pal_a <-colorRampPalette(c("#1B3049","#F0F5FF"))
pal_link <- colorRampPalette(c("grey70","grey80"))
num_cols_link <- 8
vcols_b <- pal_b(num_cols_link)
vcols_a <- pal_a(length(vcols_b))
vcols_link <- pal_link(num_cols_link)
#vcols_link <- brewer.pal(num_cols_link,"Dark2")#[2:num_cols_link]

for (i in ind_cores) {
  nodes_in_core_a <- g[(g$guild == str_guild_a)&(g$kcorenum == i)]$name
  nodes_in_core_b <- g[(g$guild == str_guild_b)&(g$kcorenum == i)]$name
  df_cores[i,]$species_guild_a <- list(unlist(lapply(nodes_in_core_a, function(x) strsplit(x,str_guild_a)[[1]][[2]])))
  df_cores[i,]$num_species_guild_a <- length(nodes_in_core_a)
  df_cores[i,]$species_guild_b <- list(unlist(lapply(nodes_in_core_b, function(x) strsplit(x,str_guild_b)[[1]][[2]])))
  df_cores[i,]$num_species_guild_b <- length(nodes_in_core_b)
}

max_species_cores <- max(df_cores[kcoremax,]$num_species_guild_a,df_cores[kcoremax,]$num_species_guild_b)
color_guild_a <- c("#4169E1","#00008B")
color_guild_b <- c("#F08080","#FF0000")
scaling_factor <- length(g)%/%100
num_a_coremax <- df_cores[kcoremax,]$num_species_guild_a


base_width <- 1200 #* (1 + 0.125* (num_a_coremax-2))
ymax <- base_width*(1+kcoremax/10)

#/aspect_ratio
basic_unit <- max(25,ymax/(4*(max(12,df_cores[2,]$num_species_guild_b, df_cores[2,]$num_species_guild_a))))
#height_y <- (5+kcoremax)*basic_unit/aspect_ratio
tot_width <- ymax * (1+0.25*(kcoremax - 2))
# yoffset <- max(df_cores[2,]$num_species_guild_b, df_cores[2,]$num_species_guild_a)*height_y*1.3
# base_width <- base_width * (1 + (num_a_coremax-2))
# tot_width <- ((kcoremax >3 ) + 1) * base_width
#ymax <- ymax + yoffset
height_y <- ymax/20
species_in_core2_a <- sum(df_cores[2:(kcoremax-1),]$num_species_guild_a)
species_in_core2_b <- sum(df_cores[2:(kcoremax-1),]$num_species_guild_b)
if (species_in_core2_a+species_in_core2_b < 6)
  height_y <- 0.6*ymax/(max(species_in_core2_a,species_in_core2_b))

maxincore2 <- max(df_cores[2,]$num_species_guild_a,df_cores[2,]$num_species_guild_b)
yoffset <- 0.05*ymax+ height_y*maxincore2* (1 + 0.15*(maxincore2)%/%3)
fmult <- (ymax+yoffset)/ymax
ymax <- ymax*fmult
tot_width <- tot_width*fmult/aspect_ratio

# if (ymax > tot_width)
#   tot_width <- (16/9)*ymax
hop_x <- 0.95*(tot_width)/(kcoremax-2)
lado <- min(0.05*tot_width,height_y * aspect_ratio)
basey <- 0.1*ymax
basex <- -0.15*hop_x
topxa <- max(0.85*hop_x,height_y)#min(0.85*hop_x,(0.5+(1/aspect_ratio))*hop_x)
miny <- ymax
maxy_zig <- 0
maxy_zig_a <- 0
maxy_zig_b <- 0
topy <- 0.3*ymax+basey
strips_height <- (ymax-yoffset)/(kcoremax-2)
last_ytail_a[kcoremax]<- topy
last_xtail_a[kcoremax]<- topxa
list_dfs_a[[kcoremax]] <- draw_coremax_triangle(basex,topxa,basey,topy,
                                                num_a_coremax,color_guild_a,
                                                df_cores$species_guild_a[[kcoremax]],
                                                g, str_guild_a, orderby = "kdegree")
p <- ggplot() +
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=list_dfs_a[[kcoremax]], mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill = list_dfs_a[[kcoremax]]$col_row,  color="transparent",alpha=alpha_level) +
  geom_text(data=list_dfs_a[[kcoremax]], aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=list_dfs_a[[kcoremax]]$label), 
            color = list_dfs_a[[kcoremax]]$col_row, size=labels_size, alpha = 1)
num_b_coremax <- df_cores[kcoremax,]$num_species_guild_b
basey <- - basey
topxb <- topxa 
topy <- - topy

list_dfs_b[[kcoremax]] <- draw_coremax_triangle(basex,topxb,basey,topy,num_b_coremax,
                                                color_guild_b,
                                                df_cores$species_guild_b[[kcoremax]],
                                                g, str_guild_b, orderby = "kdegree")
last_ytail_b[kcoremax]<- topy
last_xtail_b[kcoremax]<- topxb
p <- p + geom_rect(data=list_dfs_b[[kcoremax]] , mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                   fill = list_dfs_b[[kcoremax]]$col_row,  color="transparent", alpha=alpha_level) +
         geom_text(data=list_dfs_b[[kcoremax]] , aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, 
                   label=list_dfs_b[[kcoremax]]$label), 
                   color = list_dfs_b[[kcoremax]]$col_row, size=labels_size)

pointer_x <- max(topxa, topxb)+hop_x
pointer_y <- ymax+abs(basey)
width_zig <- 0.3*hop_x
primerkcore <- TRUE

for (kc in seq(from = kcoremax-1, to = 2))
{
  print(paste("kcoreent",kc))
  if (sum(df_cores[kc,]$num_species_guild_a,df_cores[kc,]$num_species_guild_b)>0)
  {
    pointer_x <- (kcoremax-kc)*hop_x
    pointer_y <- pointer_y - (0.5*(kcoremax-1-kc)/kcoremax)*strips_height
  }
  
  if (df_cores[kc,]$num_species_guild_a>0){
    despl_pointer_y <- displace_y_a[kc] * ymax #+ 3*as.integer((kc>2) & (kc<kcoremax))*(df_cores[kc-1,]$num_species_guild_a)*height_y
    if ((kc == 2) )
    {
      pointer_y <- yoffset+abs(basey)
      edge_core <- "yes"
    }
    else {
      edge_core <- "no"
      if (primerkcore)
        pointer_y <- ymax
      else if ((df_cores[kc-1,]$num_species_guild_a>5) & !(primerkcore)){
        pointer_y <- pointer_y - (0.8+0.1*(kcoremax-kc)/kcoremax)*strips_height
        pointer_x <- 1.1*pointer_x
      }
    }
    print(paste("kcore",kc,"zig position", pointer_y, "ymax", ymax))
    pystep <- height_y * (1 + 0.15*(length(df_cores$species_guild_a[[kc]])%/%3))
    zig <-  draw_ziggurat(g, basex = pointer_x, widthx = (1+(kc/kcoremax))*width_zig, 
                          basey = pointer_y + despl_pointer_y, ystep = pystep, strlabels = df_cores$species_guild_a[[kc]],
                          strguild = str_guild_a,
                          sizelabels = labels_size, colorb = color_guild_a, numboxes = df_cores[kc,]$num_species_guild_a, 
                          zinverse = "yes", edge = edge_core, grafo = p)
    p <- zig["p"][[1]]
    list_dfs_a[[kc]] <- zig["dr"][[1]]
    last_xtail_a[[kc]] <- max(list_dfs_a[[kc]]$x2)
    last_ytail_a[[kc]] <- -max(abs(list_dfs_a[[kc]]$y2))
    maxy_zig_a <- -max(maxy_zig_a, abs(min(list_dfs_a[[kc]]$y2)))    
    maxy_zig <-  max(maxy_zig_a, maxy_zig_b)
    primerkcore <- FALSE
    x <- c(0)
    y <- c(ymax)
  }
  if (df_cores[kc,]$num_species_guild_b>0){
    despl_pointer_y <- displace_y_b[kc] * ymax #+ 3* as.integer((kc>2) & (kc<kcoremax))*(df_cores[kc-1,]$num_species_guild_a)*height_y
    if ((kc == 2))
    {
      pointer_y <- yoffset+abs(basey)
      edge_core <- "yes"
    }
    else {
      edge_core <- "no"
      if (primerkcore)
        pointer_y <- ymax
      else if ((df_cores[kc-1,]$num_species_guild_b>5) & !(primerkcore)) {
        pointer_y <- pointer_y - 0.2*height_y*df_cores[kc,]$num_species_guild_b
        pointer_x <- 1.1*pointer_x
      }
    }
    print(paste("kcore",kc,"zig position", pointer_y, "ymax", ymax))
    pystep <- height_y * (1 + 0.1*(length(df_cores$species_guild_b[[kc]])%/%3))
    
    zig <-  draw_ziggurat(g, basex = pointer_x, widthx = (1+(kc/kcoremax))* width_zig, 
                          basey = pointer_y + despl_pointer_y,  ystep = pystep, strlabels = df_cores$species_guild_b[[kc]],
                          strguild = str_guild_b, sizelabels = labels_size,
                          colorb = color_guild_b, numboxes = df_cores[kc,]$num_species_guild_b, 
                          zinverse = "no", edge = edge_core, grafo = p)
    p <- zig["p"][[1]]
    list_dfs_b[[kc]]<- zig["dr"][[1]]
    last_xtail_b[[kc]] <- max(list_dfs_b[[kc]]$x2)
    last_ytail_b[[kc]] <- max(abs(list_dfs_b[[kc]]$y2))
    maxy_zig_b <- max(maxy_zig_b,max(list_dfs_b[[kc]]$y2))    
    maxy_zig <-  max(maxy_zig_a, maxy_zig_b)
    primerkcore <- FALSE
  }
}

mtxlinks <- data.frame(get.edgelist(result_analysis$graph))
names(mtxlinks) <- c("guild_a","guild_b")
if (length(grep(str_guild_b,mtxlinks[1,1]))>0)
  names(mtxlinks) <- rev(names(mtxlinks))

orphans_a <- df_cores$species_guild_a[[1]]
orphans_b <- df_cores$species_guild_b[[1]]

df_orph_a <- find_orphans(mtxlinks,orphans_a,g,guild_a="yes")
df_orph_b <- find_orphans(mtxlinks,orphans_b,g,guild_a="no")

gap <-  5*height_y

# Species of core 1 linked to max core (except the most generalist)

leftjump <- 0.3*hop_x
point_x <- list_dfs_a[[kcoremax]][nrow(list_dfs_a[[kcoremax]]),]$x2 - leftjump
point_y <- maxy_zig*0.75
long_tail_a <- df_orph_a[(df_orph_a$kcore == kcoremax) & (df_orph_a$repeated == "no"),]
if (length(long_tail_a)>0)
{
  v<-  draw_edge_tails(p,kcoremax,long_tail_a,list_dfs_b,color_guild_a, inverse = "yes", 
                       vertical = "no", orientation = "South", revanddrop = "yes",
                       pbackground = "no", tspline = "vertical", joinchars=joinstr)
  p <- v["p"][[1]]
  last_xtail_b[kcoremax] <- v["lastx"][[1]]
  last_ytail_b[kcoremax] <- v["lasty"][[1]]
}
point_x <- list_dfs_b[[kcoremax]][nrow(list_dfs_b[[kcoremax]]),]$x2 - leftjump #- leftjump
point_y <- -maxy_zig*0.75
print(point_y)
long_tail_b <- df_orph_b[(df_orph_b$kcore == kcoremax) & (df_orph_b$repeated == "no"),]
if (length(long_tail_b)>0){
  v<-  draw_edge_tails(p,kcoremax,long_tail_b,list_dfs_a,color_guild_b, inverse = "no", 
                       vertical = "no", orientation = "North", revanddrop = "yes",
                       pbackground = "no", tspline = "vertical", joinchars=joinstr)
  p <- v["p"][[1]]
  last_xtail_a[kcoremax] <- v["lastx"][[1]]
  last_ytail_a[kcoremax] <- v["lasty"][[1]]
}
print("last xtail a")
print(last_xtail_a)
print("last xtail b")
print(last_xtail_b)

fat_tail_x <- min(last_xtail_a[[kcoremax]],last_xtail_b[[kcoremax]],list_dfs_a[[kcoremax]][1,]$x1,list_dfs_b[[kcoremax]][1,]$y2)

max_b_kdegree <- list_dfs_b[[kcoremax]][which(list_dfs_b[[kcoremax]]$kdegree == max(list_dfs_b[[kcoremax]]$kdegree)),]$label
fat_tail_a <- df_orph_a[(df_orph_a$partner == max(max_b_kdegree)) & (df_orph_a$repeated == "no"),]

max_a_kdegree <- list_dfs_a[[kcoremax]][which(list_dfs_a[[kcoremax]]$kdegree == max(list_dfs_a[[kcoremax]]$kdegree)),]$label
fat_tail_b <- df_orph_b[(df_orph_b$partner == max(max_a_kdegree)) & (df_orph_b$repeated == "no"),]

# Fat tails - nodes of core 1 linked to most generalist of opposite guild. Left side of panel


fgap <- 0.2*tot_width #,min(list_dfs_a[[kcoremax]][1,]$x1,list_dfs_b[[kcoremax]][1,]$x1))
pos_tail_x <- min(last_xtail_a[[kcoremax]],last_xtail_b[[kcoremax]],list_dfs_b[[kcoremax]][1,]$x1-2*fgap,list_dfs_a[[kcoremax]][1,]$x1-2*fgap)

if (nrow(fat_tail_a)>0){
  pos_tail_y <- (list_dfs_b[[kcoremax]][1,]$y2+list_dfs_b[[kcoremax]][1,]$y1)/3
  v<- draw_tail(p,fat_tail_a,lado,color_guild_a[1],gen_sq_label(fat_tail_a$orph),
                aspect_ratio,pos_tail_x,pos_tail_y,fgap,pintalinks,
                lxx2 = list_dfs_b[[kcoremax]][1,]$x1, lyy2 =list_dfs_b[[kcoremax]][1,]$y1-3*lado,
                sqinverse = "yes", background = "no")
  p <- v["p"][[1]]
}

if (nrow(fat_tail_b)>0){
  pos_tail_y <- (list_dfs_a[[kcoremax]][1,]$y2+list_dfs_a[[kcoremax]][1,]$y1)/3
  
  v<- draw_tail(p,fat_tail_b,lado,color_guild_b[1],gen_sq_label(fat_tail_b$orph),
                aspect_ratio,pos_tail_x,pos_tail_y,fgap,pintalinks,
                lxx2 = list_dfs_a[[kcoremax]][1,]$x1, lyy2 = list_dfs_a[[kcoremax]][1,]$y2-3*lado,
                sqinverse = "no", background = "no")
  p <- v["p"][[1]]
}


# Nodes of core 1 linked to species in cores kcoremax-1 to core 2.

if (kcoremax >2)
{
  for (kc in seq(from = kcoremax-1, to = 2))
  {
    print(paste("KC ",kc))
    
    if (length(list_dfs_b[[kc]])>0)
      point_x <- list_dfs_b[[kc]][nrow(list_dfs_b[[kc]]),]$x2
    
    point_y <- (list_dfs_b[[kc]][1,]$y1+list_dfs_b[[kc]][1,]$y2)/2
    long_tail_a <- df_orph_a[(df_orph_a$kcore == kc) & (df_orph_a$repeated == "no"),]
    if (length(long_tail_a)>0){
      
      v<-  draw_edge_tails(p,kc,long_tail_a,list_dfs_b,color_guild_a, 
                           inverse = "no", joinchars = joinstr,pbackground = "no",
                           tspline = "lshaped")
      p <- v["p"][[1]]
      if (kc>1){
        if (length(v["lastx"][[1]])>0)
          last_xtail_b[kc] <- v["lastx"][[1]]
        if (length(v["lasty"][[1]])>0)
          last_ytail_b[kc] <-v["lasty"][[1]]
      }
    }
    
    if (length(list_dfs_a[[kc]])>0)
      point_x <- list_dfs_a[[kc]][nrow(list_dfs_a[[kc]]),]$x2
    point_y <- 1.05*list_dfs_a[[kc]][1,]$y1
    long_tail_b <- df_orph_b[(df_orph_b$kcore == kc) & (df_orph_b$repeated == "no"),]
    if (length(long_tail_b)>0){
      v<-  draw_edge_tails(p,kc,long_tail_b,list_dfs_a,color_guild_b, 
                           inverse = "yes", joinchars = joinstr,pbackground = "no",
                           tspline = "lshaped")
      p <- v["p"][[1]]
      if (kc>1){
        if (length(v["lastx"][[1]])>0)
          last_xtail_a[kc] <- v["lastx"][[1]]
        if (length(v["lasty"][[1]])>0)
          last_ytail_a[kc] <-v["lasty"][[1]]
        print(paste("kc",kc,"last_xtail_a[kc]",last_xtail_a[kc],"last_ytail_a[kc]",last_ytail_a[kc]))
      }
    }
  }
}

weirds_a <-  df_orph_a[df_orph_a$repeated== "yes",]
weirds_a <-  weirds_a[rev(order(weirds_a$orph,weirds_a$kcore)),]
weirds_b <-  df_orph_b[df_orph_b$repeated== "yes",]
weirds_b <-  weirds_b[rev(order(weirds_b$orph,weirds_b$kcore)),]
if (nrow(weirds_a)>0)
  weirds_a$drawn <- "no"
if (nrow(weirds_b)>0)
  weirds_b$drawn <- "no"

# Create empty df_chains data frame

if ((nrow(weirds_a)>0) | (nrow(weirds_b)>0)){
  original_weirds_a <- weirds_a
  original_weirds_b <- weirds_b
  df_chains <- data.frame(x1 = numeric(0), x2 = numeric(0), y1 = numeric(0), y2 = numeric(0),
                          guild = character(0), orph = integer(0), partner = integer(0), 
                          kcorepartner = integer(0), xx2 = numeric(0), yy2 = numeric(0), stringsAsFactors = FALSE )
  
  while(nrow(weirds_a)+nrow(weirds_b)>0)
    {
    if (nrow(weirds_a)>0){
      k <- store_root_leaf(weirds_a, df_chains, str_guild_a, lado, gap)
      df_chains <- k["df_chains"][[1]]
      weirds_a <- k["weirds"][[1]]
    }
    if (nrow(weirds_b)>0){
      k <- store_root_leaf(weirds_b, df_chains, str_guild_b, lado, gap)
      df_chains <- k["df_chains"][[1]]
      weirds_b <- k["weirds"][[1]]
    }
    
    if (nrow(weirds_a)>0){
      k <- store_branch_leaf(weirds_a, weirds_b, df_chains, str_guild_a, lado, gap)
      df_chains <- k["df_chains"][[1]]
      weirds_a <- k["weirds"][[1]]
      weirds_b <- k["weirds_opp"][[1]]
    }
    if (nrow(weirds_b)>0){
      k <- store_branch_leaf(weirds_b, weirds_a, df_chains, str_guild_b, lado, gap)
      df_chains <- k["df_chains"][[1]]
      weirds_b <- k["weirds"][[1]]
      weirds_a <- k["weirds_opp"][[1]]
    }
    
    # Now they may be some weirds of core 1 linked to core 1 that were not
    # stored in the previous procedure
    
    weirds_a <- weirds_a[weirds_a$drawn == "no",]
    weirds_b <- weirds_b[weirds_b$drawn == "no",]  
    
  }
  
  
  p <- draw_weird_chains(p, df_chains, lado, pintalinks)
}

print("weirds_a")
print(weirds_a)
print("weirds_b")
print(weirds_b)

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
            bend_line = "no"
            if (((kc == 2) & (kcb == kcoremax)) | ((kc == kcoremax) & (kcb == 2)))
              bend_line = "horizontal"
              
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
              bend_line = "arc"
              lcolor = "pink"
            }
            else if (kc > kcb) {
              if (kc == kcoremax)
                link <- data.frame(x1=c((list_dfs_a[[kc]][j,]$x2 + list_dfs_a[[kc]][j,]$x1)/2), 
                                   x2 = c(list_dfs_b[[kcb]][i,]$x1), 
                                   y1 = c(list_dfs_a[[kc]][j,]$y2),  y2 = c(list_dfs_b[[kcb]][i,]$y1) )
              else{
                link <- data.frame(x1=c(list_dfs_a[[kc]][j,]$x2), 
                                   x2 = c(list_dfs_b[[kcb]][i,]$x1), 
                                   y1 = c(list_dfs_a[[kc]][j,]$y1),  y2 = c(list_dfs_b[[kcb]][i,]$y1) )
                bend_line = "diagonal"
              }
              lcolor = "green"
            }
            else
            {
              if (kcb == kcoremax){
                y_2 <- c(list_dfs_b[[kcb]][i,]$y2)
                x_2 <- c((list_dfs_b[[kcb]][i,]$x2 + list_dfs_b[[kcb]][i,]$x1)/2)
              }
              else{
                y_2 <- c(list_dfs_b[[kcb]][i,]$y1)
                x_2 <- c(list_dfs_b[[kcb]][i,]$x2)
                
              }
              link <- data.frame(x1=c(list_dfs_a[[kc]][j,]$x1), 
                                 x2 = x_2, 
                                 y1 = c(list_dfs_a[[kc]][j,]$y1),  y2 = y_2)
              lcolor = "blue" 
            }

            p <- draw_link(p, xx1=link$x1, xx2 = link$x2, 
                           yy1 = link$y1, yy2 = link$y2, 
                           slink = size_link, clink = color_link, # clink = sample(vcols_link,1), 
                           alpha_l = alpha_link , spline = bend_line)

          }
        }
      }      
    }
  }
}
p <- p+ ggtitle(sprintf("Network %s ", network_name))
p <- p + coord_fixed(ratio=aspect_ratio) +theme_bw() + theme(panel.grid.minor.x = element_blank(),
                                                             panel.grid.minor.y = element_blank(),
                                                             panel.grid.major.x = element_blank(),
                                                             panel.grid.major.y = element_blank(),
                                                             plot.title = element_text(lineheight=.8, face="bold"))

landmark_top <- 1.1*ymax
landmark_top <- 1*landmark_top
mlabel <- "."
p <- p +annotate(geom="text", x=0, 
                 y=landmark_top, 
                label=mlabel, 
                 colour = "red", size=2, hjust = 0, vjust = 0, angle = 0,  
                 guide =FALSE)
landmark_right <- tot_width+1.5*hop_x
p <- p +annotate(geom="text", x= landmark_right, y=0, label=mlabel, 
                 colour = "red", size=2, hjust = 0, vjust = 0, angle = 0,  
                 guide =FALSE)
landmark_left <- min(last_xtail_a[[kcoremax]],last_xtail_b[[kcoremax]])-1.1*hop_x
landmark_left <- 0.6* landmark_left
p <- p +annotate(geom="text", x=landmark_left, y=0, label=mlabel, 
                 colour = "red", size=2, hjust = 0, vjust = 0, angle = 0,  
                 guide =FALSE)


if (print_to_file){
  ppi <- 600
  png(paste0(network_name,"_almond.png"), width=(16*ppi), height=9*ppi, res=ppi)
}
  print(p)
if (print_to_file){
  dev.off()
}