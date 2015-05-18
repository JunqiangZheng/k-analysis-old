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
      ssal <- gsub("  "," ",paste(ssal,joinchars))
  }
  return(ssal)
}

draw_square<- function(grafo,basex,basey,side,fillcolor,alphasq,labelcolor,
                       langle,hjust,vjust,slabel,aspect_ratio,lbsize = labels_size,
                       inverse="no",adjustoxy = "yes", edgescolor="transparent")
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
                   colour = labelcolor, size=lbsize, hjust = hjust, 
                   vjust = vjust, angle = langle,  
                   guide =FALSE)
  
  return(p)
}

draw_rectangle<- function(basex,basey,widthx,widthy,grafo,bordercolor,fillcolor,palpha,slabel,inverse="no",sizelabel=3)
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
                         fill = fillcolor, alpha =palpha, color=bordercolor, size = 0.025, linetype = 3)
  p <- p +annotate(geom="text", x=x1+(x2-x1)/8, y=signo*(y1+(y2-y1)/2), label=slabel, 
                   colour = fillcolor, size=sizelabel, hjust = 0,  guide =FALSE)
  
  return(p)
}

draw_core_box <- function(grafo, kcore)
{
  marginy <- ifelse((df_cores$num_species_guild_a[2]+df_cores$num_species_guild_b[2]>4),height_y,0.7*height_y)
  marginx <- lado
  if (kcore<kcoremax)
  {
    x_inf <- min(list_dfs_b[[kcore]]$x1,list_dfs_a[[kcore]]$x1) - marginx
    widthx <- (max(list_dfs_b[[kcore]]$x2,list_dfs_a[[kcore]]$x2 ) - x_inf) + marginx
    y_inf <- ifelse(length(list_dfs_a[[kcore]])>0, min(list_dfs_a[[kcore]]$y2,list_dfs_a[[kcore]]$y1) - marginy,
                    min(list_dfs_b[[kcore]]$y2,list_dfs_b[[kcore]]$y1) - marginy)
    widthy <- ifelse(length(list_dfs_b[[kcore]])>0, max(list_dfs_b[[kcore]]$y2) - y_inf + (1+0.45*kcoremax)*marginy,
                     widthy <- max(list_dfs_a[[kcore]]$y1) - y_inf + (1+0.45*kcoremax)*marginy)  
  }
  else{
    x_inf <- min(list_dfs_b[[kcore]]$x1,list_dfs_a[[kcore]]$x1) - 3*marginx
    widthx <- (max(list_dfs_b[[kcore]]$x2,list_dfs_a[[kcore]]$x2 ) - x_inf) + marginx
    y_inf <- min(list_dfs_a[[kcore]]$y2,list_dfs_b[[kcore]]$y2) - marginy
    widthy <- max(list_dfs_a[[kcore]]$y2) - y_inf + 2*marginy
  }
  divcolor <- corecols[kcore]
  p <- draw_rectangle(x_inf,y_inf,widthx,widthy,grafo,divcolor,"transparent",0.01,"",inverse="no",sizelabel = labels_size)
  if (kcore == kcoremax){
    position_x_text <- x_inf+1.5*marginx
    corelabel <- paste("","core",kcore)
  }
  else{
    position_x_text <- x_inf-marginx+widthx/2
    corelabel <- paste("core",kcore)
  }
  position_y_text <- y_inf+widthy - 1.5*marginx
  max_position_y_text_core <- max(max_position_y_text_core,position_y_text)
  if (kcore != kcoremax){
    px <- position_x_text
    py <- position_y_text
    pangle <- 0
  }
  else {
    px <- position_x_text+1.2*marginx/2
    py <- 0
    ifelse (flip_results,pangle<-0,pangle <- 90)
  }
  p <- p +annotate(geom="text", x=px, y=py, label=corelabel, colour = divcolor, 
                   size=lsize_core_box+0.1*kcore, hjust = 0, vjust = 0, angle = pangle, guide =FALSE)
  calc_vals <- list("p" = p, "max_position_y_text_core" = max_position_y_text_core) 
  return(calc_vals) 
}

draw_tail <- function(p,fat_tail,lado,color,sqlabel,aspect_ratio,basex,basey,gap,
                      paintlinks,lxx2=0,lyy2=0,sqinverse = "no", 
                      position = "West", background = "no", 
                      first_leaf = "yes", spline = "no", psize = lsize_kcore1)
{
  adjust <- "yes"
  lvjust <- 0
  lhjust <- 0
  langle <- 0
  ecolor <- "transparent"
  bgcolor <- color
  labelcolor <- color
  palpha <- alpha_link
  sidex <- lado*(0.5+sqrt(nrow(fat_tail)))
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
  else if ((position == "North") |(position == "South")) {
    xx <- basex-sidex/2
    posxx1 <- xx+sidex/2
    posyy1 = signo*(yy)
  }

  if (background == "no")
  {
    ecolor <- "transparent"
    palpha <- alpha_level-0.12
    rot_angle <-30
    if (position == "North") 
    {
      langle <- rot_angle
    }
    if (position == "South") 
    {
      langle <- -rot_angle
      lvjust <- 0
    }
    if (position == "West"){
      adjust <- "yes"
    }
  }
  if ((flip_results) & (langle == 0) & (position!="West") )
    langle <- langle + 70
  p <- draw_square(p,xx,yy,paintsidex,bgcolor,palpha,labelcolor,langle,lhjust,lvjust,
                   slabel=sqlabel,aspect_ratio,lbsize = psize,inverse = sqinverse, 
                   adjustoxy = adjust, edgescolor = ecolor)
  if (paintlinks){
    p <- draw_link(p, xx1=posxx1, xx2 = plxx2, 
                   yy1 = posyy1, yy2 = plyy2, 
                   slink = size_link, clink = c(color_link), 
                   alpha_l = alpha_link , spline= spline)
  }
  calc_vals <- list("p" = p, "sidex" = sidex, "xx" = posxx1, "yy" = posyy1) 
  return(calc_vals)
}


draw_edge_tails <- function(p,point_x,point_y,kcoreother,long_tail,list_dfs,color_guild, inverse = "no", 
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
        rxx <- point_x*(0.9+0.1*horiz_kcoremax_tails_expand)*kcore1tail_disttocore[1]
        ryy <- point_y*0.9*kcore1tail_disttocore[2]
        data_row <- list_dfs[[kcoreother]][which(list_dfs[[kcoreother]]$label == i),]
        xx2 <- (data_row$x2+data_row$x1)/2
        yy2 <- data_row$y2
      }
      if (kcoreother == 2) {
        yy2 <- ifelse (kcoremax > 2, (data_row$y1 + data_row$y2)/2, data_row$y2)
        point_y <- (kcore2tail_vertical_separation-1)*sign(point_y)*height_y + point_y
        ryy <- point_y
      }
      else if (kcoreother<kcoremax) {
        point_y <- (innertail_vertical_separation-1)*sign(point_y)*height_y + point_y
        ryy <- point_y
      }
      v<- draw_tail(p,little_tail,lado,color_guild[1],
                    gen_sq_label(little_tail$orph,joinchars = " "),
                    aspect_ratio,rxx,ryy,gap,paintlinks,lxx2 = xx2,
                    lyy2 = yy2, sqinverse = inverse, position = orientation,
                    background = pbackground, spline = tspline, psize = lsize_kcore1)
      p <- v["p"][[1]]
      rxx <- v["xx"][[1]]
      ryy <- v["yy"][[1]]
      if (vertical == "yes"){
        salto <- v["sidex"][[1]]/aspect_ratio
        point_y <- point_y + 1.4*signo*salto
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

conf_outsiders <- function(outsiders,basex,basey,sidex,fillcolor,strguild)
{
  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  r <- c()
  col_row <- c()
  numboxes <- length(outsiders)
  pbasex <- basex  
  xstep <- 1.5*sidex
  if (numboxes<10)
  {
    xsep <- 2.5*outsiders_separation_expand
    if (outsiders_separation_expand < 10)
      ysep <- outsiders_separation_expand
    else
      ysep <- 5*outsiders_separation_expand
  }
  else
  {
    xsep <- 5*outsiders_separation_expand
    ysep <- 15*outsiders_separation_expand
  }
  
  for (j in (1:numboxes))
  {
    x1 <- c(x1, pbasex+(j*xsep*xstep))
    x2 <- c(x2, x1[j]+xstep)
    y1 <- c(y1, basey-ysep*xstep/aspect_ratio)
    y2 <- c(y2, y1[j]-xstep/aspect_ratio)
    r <- c(r,j)
    col_row <- c(col_row,fillcolor)
  }
  d1 <- data.frame(x1, x2, y1, y2, r, col_row)
  d1$label <- ""
  for (i in 1:length(outsiders))
    d1[i,]$label <- strsplit(outsiders[i],strguild)[[1]][2]
  return(d1)
}


draw_sq_outsiders <- function(p,dfo,paintsidex,alpha_level,aspect_ratio,lsize)
{
  p <- p + geom_rect(data=dfo, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                     fill = dfo$col_row, alpha = alpha_level,color="transparent") +
    geom_text(data=dfo, aes(x=(x2+x1)/2, y= (y2+y1)/2), color=dfo$col_row, 
              label = dfo$label, size=lsize, vjust=0.5)
  return(p)
}

handle_outsiders <- function(p,outsiders,df_chains) {
  if (length(outsider)>0){
    paintsidex <- 2*height_y*aspect_ratio
    outsiders_a <- outsider$name[grep(str_guild_a,outsider$name)]
    outsiders_b <- outsider$name[grep(str_guild_b,outsider$name)]
    pox <- -(hop_x/4)+ tot_width * (displace_outside_component[1]-1)
    poy <- min(-last_ytail_b[!is.na(last_ytail_b)]-4*lado,df_chains$y1) * displace_outside_component[2]
    dfo_a <- conf_outsiders(outsiders_a,pox,poy,lado,color_guild_a[1],str_guild_a)
    dfo_b <- conf_outsiders(outsiders_b,pox,poy-5*lado/aspect_ratio,lado,color_guild_b[1],str_guild_b)
    p <- draw_sq_outsiders(p,dfo_a,paintsidex,alpha_level,aspect_ratio,lsize_kcore1)  
    p <- draw_sq_outsiders(p,dfo_b,paintsidex,alpha_level,aspect_ratio,lsize_kcore1)
    for (j in 1:nrow(dfo_a))
    {
      mtxa <- mtxlinks[which(mtxlinks$guild_a == paste0(str_guild_a,dfo_a[j,]$label)),]
      for (i in 1:nrow(dfo_b))
      {
        if (sum(as.character(mtxa$guild_b) == paste0(str_guild_b,dfo_b[i,]$label))>0)
        {
          bend_line = "no"
          link <- data.frame(x1=c(dfo_a[j,]$x1 + (dfo_a[j,]$x2-dfo_a[j,]$x1)/2), 
                             x2 = c(dfo_b[i,]$x1 +(dfo_b[i,]$x2-dfo_b[i,]$x1)/2), 
                             y1 = c(dfo_a[j,]$y2),  y2 = c(dfo_b[i,]$y1) )
          lcolor = "orange"
          p <- draw_link(p, xx1=link$x1, xx2 = link$x2, 
                         yy1 = link$y1, yy2 = link$y2, 
                         slink = size_link, clink = c(color_link), 
                         alpha_l = alpha_link , spline = bend_line) 
        }
        
      }
    }  
    
    
    margin <- height_y
    
    x_inf <- min(dfo_a$x1,dfo_b$x1) - 1.5*margin
    widthx <- max(dfo_a$x2,dfo_b$x2) - x_inf + margin
    y_inf <- min(dfo_a$y2,dfo_b$y2) - 2*margin/aspect_ratio
    widthy <- max(dfo_a$y2,dfo_b$y2) - y_inf + 2*margin/aspect_ratio
    
    divcolor <- "grey70"
    p <- draw_rectangle(x_inf,y_inf,widthx,widthy,p,divcolor,"transparent",0.02,"",inverse="no",sizelabel = labels_size)
    position_x_text <- x_inf+20
    corelabel <- paste("Outside the giant component")
    position_y_text <- y_inf + margin/aspect_ratio + (0.9+0.2*outsiders_separation_expand)*widthy
    px <- position_x_text
    py <- position_y_text
    if (flip_results){
      px <- x_inf + widthx + margin * outsiders_separation_expand
      py <- y_inf
    }
      
    p <- p +annotate(geom="text", x=px, y=py, label=corelabel, colour = divcolor, 
                     size=3, hjust = 0, vjust = 0, angle = 0, guide =FALSE)
    
  }
  return(p)
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
  pbasex <- coremax_triangle_width_factor*( basex - (numboxes %/%8) * abs(topx-basex)/3)
  
  xstep <- (topx-pbasex)*1/numboxes
  
  ptopy <- topy * coremax_triangle_height_factor
  ystep <- (ptopy-basey)*0.7/numboxes
  for (j in (1:numboxes))
  {
    x1 <- c(x1, pbasex+(j-1)*xstep)
    x2 <- c(x2, x1[j]+0.9*xstep)
    y1 <- c(y1, basey)
    y2 <- c(y2, ptopy-(j-1)*ystep)
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
  yjump <- 0.2*height_y
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
  }
  for (j in (1:numboxes))
  {
    x1 <- c(x1, basex+(j-1)* (xstep/2))
    if (edge_tr == "yes")
      x2 <- c(x2, topx-(j-1)*(xstep/8)  )
    else
      x2 <- c(x2, topx-(j-1)*xstep/4)
    y1 <- c(y1, basey-(j-1)*(ystep*fmult_hght+yjump)  )
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

draw_individual_ziggurat <- function(igraphnet, basex = 0, widthx = 0, basey = 0, ystep = 0, numboxes = 0, strlabels = "", strguild = "",
                          sizelabels = 3, colorb = "", grafo = "", zinverse ="no", edge = "no", angle = 0)
{
  dr <- conf_ziggurat(igraphnet, basex,widthx,basey,ystep,numboxes,colorb, strlabels, 
                      strguild, inverse = zinverse, edge_tr = edge)
  p <- grafo + geom_rect(data=dr, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                         fill = dr$col_row, alpha = alpha_level,color="transparent")
  if (displaylabelszig)
    p <- p + geom_text(data=dr, aes(x=x1+0.3*(x2-x1)/2+0.7*((max(r)-r)%%2)*(x2-x1), 
                                    y= (y2+y1)/2), color=dr$col_row, 
                       label = dr$label, size=lsize_zig, vjust=0.3)
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
  if (!is.null(orphans))
    for (i in orphans)
    {
      if (guild_a == "yes")
      {
        partner <- mtxlinks$guild_b[(mtxlinks$guild_a == paste0(str_guild_a,i))]
        str_opp <- str_guild_b
        str_own <- str_guild_a
      }
      else{
        partner <- mtxlinks$guild_a[(mtxlinks$guild_b == paste0(str_guild_b,i))]
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
      df_orph <- df_orph[!is.na(df_orph$orph),]
    }
  return(df_orph)
}

draw_link <- function(grafo, xx1 = 0,xx2 = 0,yy1 = 0,yy2 = 0,
                      slink = 1,clink = c("gray70"),alpha_l = 0.1, spline = "no")
{
  link <- data.frame(x1=xx1, x2 = xx2, y1 = yy1,  y2 = yy2)
  col_link = sample(clink,1)
  if (spline == "no")
    p <- grafo + geom_segment(data=link, aes(x=x1, y=y1, xend=x2, yend=y2), size=slink, color=col_link ,alpha=alpha_l)
  else{
    if (spline == "horizontal"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.05,link$x1+(link$x2-link$x1)*0.75,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.1,link$y1+(link$y2-link$y1)*0.65,link$y2)
    }
    else if (spline == "diagonal"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.5,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.55,link$y2)
    }
    else if (spline == "vertical"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.85,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.9,link$y2)
    }
    else if (spline == "lshaped"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.80,link$x1+(link$x2-link$x1)*0.90,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.95,link$y1+(link$y2-link$y1)*0.99,link$y2)
    }
    else if (spline == "weirdhorizontal"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.40,link$x1+(link$x2-link$x1)*0.75,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.6,link$y1+(link$y2-link$y1)*0.70,link$y2)
    }
    else if (spline == "arc"){
      x <- c(link$x1,link$x1+(link$x2-link$x1)*0.20,link$x2)
      y <- c(link$y1,link$y1+(link$y2-link$y1)*0.50,link$y2)
    }
    xout <- seq(min(x),max(x),length.out = 1000)
    s1 <- spline(x,y,xout=xout,method='natural')
    ds1 <- as.data.frame(s1)
    p <- grafo + geom_line(data =ds1,aes(x,y), size=slink, color=col_link ,alpha=alpha_l)
  }
  return(p)
}

weird_analysis <- function(weirds,opposite_weirds,species)
{
  ldf <- weirds[weirds$orph == species,]
  if (max(ldf$kcore)>1)
    return(ldf)   
}

store_weird_species <- function (row_orph, df_store, strguild, lado, gap, original_weirds_a, original_weirds_b)
{
  
  sidex <- 1.6*lado
  index <- nrow(df_store)+1
  df_store[index,]$kcorepartner <- row_orph$kcore
  
  separation <- (1+weirds_boxes_separation_count)*sidex
  tot_weirds <- nrow(original_weirds_a)+nrow(original_weirds_b)
  jumpfactor <- (4-min(3,(tot_weirds%/%10)))
  cgap <- (lado+gap/(5-min(3,(tot_weirds%/%10))))
  
  if (row_orph$kcore > 1){
    df_store$guild[index] <- as.character(strguild)
    df_store$orph[index] <- row_orph$orph
    df_store$partner[index] <- row_orph$partner
    if (strguild == str_guild_b)
      data_row <- list_dfs_a[[row_orph$kcore]][list_dfs_a[[row_orph$kcore]]$label==row_orph$partner,]
    if (strguild == str_guild_a)
      data_row <- list_dfs_b[[row_orph$kcore]][list_dfs_b[[row_orph$kcore]]$label==row_orph$partner,]
    if (row_orph$kcore == kcoremax)
    {
        if (strguild == str_guild_b){
          edge_row <- list_dfs_a[[kcoremax]][1,]
          xbase <-  min(last_xtail_a[[kcoremax]],edge_row$x1 - 2*gap) - gap
        }
        else{
          edge_row <- list_dfs_b[[kcoremax]][1,]
          xbase <-  min(last_xtail_b[[kcoremax]],edge_row$x1 - 2*gap)- gap
        }
        df_store$x1[index] <- xbase - 2 * gap 
        if (kcoremax > 2)
          df_store$y1[index] <- max(abs(edge_row$y2),abs(edge_row$y1)) + 6*cgap/(aspect_ratio)
        else{
          xbase <- 0
          df_store$y1[index] <- max(abs(edge_row$y2),abs(edge_row$y1)) + 4*cgap/(aspect_ratio)
        }
        
        if (df_store$guild[index] == str_guild_a){
          df_store$y1[index] = -abs(df_store$y1[index])
          df_store$y2[index] = -abs(df_store$y2[index])
        }
        repetitions <- sum((df_store$partner == row_orph$partner) & (df_store$guild == strguild))
        if (repetitions > 1){
          df_store$x1[index] <- df_store$x1[index] -  (repetitions-1) * sidex
          df_store$y1[index] <- df_store$y1[index] +  sign(df_store$y1[index])*(repetitions-1) * (3+as.integer(kcoremax>3)) * sidex/aspect_ratio
        }
        repetitions_root <- sum((df_store$kcorepartner == kcoremax) & (df_store$guild == strguild))
        if (repetitions_root > 1){
          df_store$x1[index] <- df_store$x1[index] +  3* (repetitions_root) * (kcoremax) * sidex
          df_store$y1[index] <- df_store$y1[index] +  sign(df_store$y1[index])*(1/jumpfactor)*((repetitions_root+ 0.7*index) * 3* sidex/aspect_ratio)
        }
      
    } else {
      if (row_orph$kcore == 2){
        xoffset <- 2*weirdskcore2_horizontal_dist_rootleaf_expand*separation   # Controls the separation of weirds root leaves connected to core 2
        yoffset <- weirdskcore2_vertical_dist_rootleaf_expand*separation/aspect_ratio
      } else{
        xoffset <- 0
        yoffset <- 0
      }
      if (strguild == str_guild_b)
      {
        data_row_pic <- list_dfs_a[[row_orph$kcore]]
        df_store$x1[index]<-  min(data_row_pic$x2) + factor_hop_x* separation + xoffset
        df_store$y1[index] <- last_ytail_a[row_orph$kcore] - (1+sqrt(kcoremax))*sidex/aspect_ratio - yoffset
        last_ytail_a[row_orph$kcore] <<- -abs(df_store$y1[index])
      }
      if (strguild == str_guild_a)
      {
        data_row_pic <- list_dfs_b[[row_orph$kcore]]
        df_store$x1[index] <- min(data_row_pic$x2) + factor_hop_x* separation + xoffset
        df_store$y1[index] <- last_ytail_b[row_orph$kcore] +  (1+sqrt(kcoremax))*sidex/aspect_ratio + yoffset
        last_ytail_b[row_orph$kcore] <<- abs(df_store$y1[index])
      }
    }
    df_store$x1[index] <- df_store$x1[index]* root_weird_expand[1]
    df_store$y1[index] <- df_store$y1[index]* root_weird_expand[2]
  }
  else {                                          # Branch leaves
    df_store$partner[index] <- row_orph$orph
    df_store$orph[index] <- row_orph$partner
    df_store$guild[index] <- as.character(strguild)
    data_row <- df_store[(df_store$orph == row_orph$orph) & (swap_strguild(strguild) == df_store$guild),]
    repetitions <- sum((df_store$partner == row_orph$orph) & (df_store$guild == strguild))

    if (data_row$kcorepartner == kcoremax){
      if (kcoremax > 2){
        df_store$x1[index] <- data_row$x1 - 1.5*separation*(repetitions)
        df_store$y1[index] <- data_row$y1 + sign(data_row$y1)*4*(weirds_boxes_separation_count*height_y + (repetitions-1)*sidex/aspect_ratio)
      }
      else{
        df_store$x1[index] <- data_row$x1 - (4+0.2*sqrt(index))*sidex
        df_store$y1[index] <- data_row$y1+ sign(data_row$y1)*(weirds_boxes_separation_count*height_y + as.integer(data_row$partner)*sidex/aspect_ratio) + 
          (2+0.2*sqrt(index))*sign(data_row$y1)*(repetitions-1)*sidex/aspect_ratio
      }
    }
    else{                                   # weirds connected to root leaf connected to kcoremax
      hjump <- sign(data_row$x1)*factor_hop_x* separation
      df_store$x1[index] <- data_row$x1 + hjump
      df_store$y1[index] <- data_row$y1 + sign(data_row$y1)*2*(repetitions-1)*sidex/aspect_ratio
    }
    reps <- sum((df_store$partner == df_store$partner[index]) & (df_store$guild == strguild))
    
    if (kcore1weirds_leafs_vertical_separation!=1){
      addjump <- sign(df_store$y1[index])*reps*kcore1weirds_leafs_vertical_separation*sidex/aspect_ratio
      df_store$y1[index]<- addjump + df_store$y1[index]
      print(paste("index",index,"df_store$y1[index]",df_store$y1[index],"reps",reps,"addjump",addjump))
    }

  }
  df_store$x2[index] <- df_store$x1[index] + sidex
  df_store$y2[index] <- df_store$y1[index] + sidex/aspect_ratio
  if (row_orph$kcore == kcoremax){
    df_store$xx2[index] <- (data_row$x2+data_row$x1)/2
    df_store$yy2[index] <- data_row$y2
  }
  else
  {
    if (df_store$kcorepartner[index] > 1){
      df_store$xx2[index] <- data_row$x2
      df_store$yy2[index] <- (data_row$y2+data_row$y1)/2
    }
    else {
      if ((data_row$x2-data_row$x1)>0)
        df_store$xx2[index] <- data_row$x1 + sidex/4
      else
        df_store$xx2[index] <- data_row$x1
      df_store$yy2[index] <- data_row$y1
    }
  }
  return(df_store)
}

draw_weird_chains <- function(grafo, df_chains, paintsidex, paintlinks)
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
    hjust <- 0
    vjust <- 0
    p <- draw_square(p,df_chains[i,]$x1,df_chains[i,]$y1,1.5*paintsidex,bgcolor,alpha_level,
                     bgcolor,0,hjust,vjust,
                     slabel=df_chains[i,]$orph,aspect_ratio,
                     lbsize = lsize_kcore1,inverse = "no")
    if (paintlinks){
      
      xx2 = df_chains[i,]$xx2
      if (df_chains[i,]$kcorepartner == kcoremax){
        xx1 = df_chains[i,]$x2-paintsidex/2
        yy1 = df_chains[i,]$y1#-sign(df_chains[i,]$y1)*(df_chains[i,]$y2-df_chains[i,]$y1)/2
      }
      else{
        xx1 = df_chains[i,]$x1
        yy1 = df_chains[i,]$y1
        yy1 <-  yy1 + paintsidex/(2*aspect_ratio)
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
                     slink = size_link, clink = c(color_link), 
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

store_root_leaf <- function(weirds,df_chains,strguild, lado, gap, original_weirds_a, original_weirds_b)
{
  for (i in 1:nrow(weirds))
  {
    if (weirds[i,]$kcore > 1){
      print(weirds[i,])
      df_chains <- store_weird_species(weirds[i,], df_chains, strguild, lado, gap, original_weirds_a, original_weirds_b)
      print(df_chains)
      weirds[i,]$drawn = "yes"
    }
  }
  calc_vals <- list("df_chains" = df_chains, "weirds" = weirds) 
  return(calc_vals)
}

store_branch_leaf <- function(weirds, weirds_opp,df_chains, pstrguild, lado, gap, original_weirds_a, original_weirds_b)
{
  for (i in 1:nrow(weirds))
  {
    if (weirds[i,]$drawn == "no"){
      strguild <- pstrguild
      if (sum( ((df_chains$orph == weirds[i,]$orph) & (df_chains$guild == strguild)) )>0 ){
        strguild <- swap_strguild(pstrguild)
        df_chains <- store_weird_species(weirds[i,], df_chains, strguild, lado, gap, original_weirds_a, original_weirds_b)
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


draw_innercores_tails <- function(p,kc,list_dfs,df_orph,color_guild, inverse="no")
{
  lastx <- 0
  lasty <- 0
  
  lpoint_x <- 0
  if (length(list_dfs[[kc]])>0)
    if (kc>2)
      lpoint_x <- list_dfs[[kc]][nrow(list_dfs[[kc]]),]$x2
  else
    lpoint_x <- list_dfs[[kc]][nrow(list_dfs[[kc]]),]$x2 + 4*lado
  if (kc>2)
    lpoint_y <- (list_dfs[[kc]][1,]$y1+list_dfs[[kc]][1,]$y2)/2
  else{
    if (kcoremax > 2)
      lpoint_y <- (list_dfs[[kc]][1,]$y1+list_dfs[[kc]][1,]$y2)/4
    else{
      kcore2tail_vertical_separation <- kcore2tail_vertical_separation + 2
      lpoint_y <- 1.5*max(list_dfs[[2]]$y2)
    }
  }
  long_tail <- df_orph[(df_orph$kcore == kc) & (df_orph$repeated == "no"),]
  if (length(long_tail)>0){
    v<-  draw_edge_tails(p,lpoint_x,lpoint_y,kc,long_tail,list_dfs,color_guild, 
                         inverse = inverse, joinchars = joinstr,pbackground = "no",
                         tspline = "lshaped")
    p <- v["p"][[1]]
    if (length(v["lastx"][[1]])>0)
      lastx <- v["lastx"][[1]]
    if (length(v["lasty"][[1]])>0)
      lasty <-v["lasty"][[1]]
  }
  calc_vals <- list("p" = p, "point_x" = lpoint_x, "point_y" = lpoint_y, "lastx" = lastx, "lasty" = lasty) 
  return(calc_vals)
}

draw_inner_links <- function(p)
{
  if (paintlinks)
  {
    for (kcb in seq(kcoremax,2))
    {
      for (kc in seq(kcoremax,2))
      {
        for (j in seq(along=list_dfs_a[[kc]]$label))
        {
          numberlinksa <- sum(mtxlinks$guild_a == paste0(str_guild_a,list_dfs_a[[kc]]$label[j]) )
          data_a <- list_dfs_a[[kc]][j,]
          foundlinksa <- 0
          for (i in seq(along =list_dfs_b[[kcb]]$label))
          {
            if (sum(mtxlinks$guild_a == paste0(str_guild_a,list_dfs_a[[kc]]$label[j]) & mtxlinks$guild_b == paste0(str_guild_b,list_dfs_b[[kcb]]$label[i]))>0)
            {
              foundlinksa <- foundlinksa + 1
              data_b <- list_dfs_b[[kcb]][i,]
              bend_line = "no"
              if (((kc == 2) & (kcb == kcoremax)) | ((kc == kcoremax) & (kcb == 2)))
                bend_line = "horizontal"  
              if ((kc == kcoremax) & (kcb == kcoremax))
              {
                link <- data.frame(x1= data_a$x1 + (data_a$x2-data_a$x1)/2, 
                                   x2 = data_b$x1 +(data_b$x2-data_b$x1)/2, 
                                   y1 = data_a$y1,  y2 = list_dfs_b[[kcb]]$y1[i] )
                lcolor = "orange"
                bend_line = "no"
              }
              else if (kc == kcb) {
                link <- data.frame(x1=  data_a$x1, 
                                   x2 = data_b$x1, 
                                   y1 = data_a$y1,  
                                   y2 = data_b$y1 )
                bend_line = "no"
                lcolor = "pink"
              }
              else if (kc > kcb) {
                if (kc == kcoremax)
                  link <- data.frame(x1= (data_a$x2 + data_a$x1)/2, 
                                     x2 = data_b$x1, 
                                     y1 = data_a$y2,  y2 = data_b$y1 )
                else{
                  link <- data.frame(x1=  data_a$x2 , 
                                     x2 = data_b$x1, 
                                     y1 = data_a$y1,  y2 = data_b$y1 )
                  bend_line = "diagonal"
                }
                lcolor = "green"
              }
              else
              {
                if (kcb == kcoremax){
                  y_2 <- data_b$y2
                  x_2 <- (data_b$x2 + data_b$x1)/2
                }
                else{
                  y_2 <- data_b$y1
                  x_2 <- data_b$x2
                  
                }
                link <- data.frame(x1= data_a$x1, 
                                   x2 = x_2, 
                                   y1 = data_a$y1,  y2 = y_2)
                lcolor = "blue" 
              }
              p <- draw_link(p, xx1=link$x1, xx2 = link$x2, 
                             yy1 = link$y1, yy2 = link$y2, 
                             slink = size_link,clink =  c(color_link), 
                             alpha_l = alpha_link , spline = bend_line)            
            }
            if (foundlinksa >= numberlinksa )
              break
          }
        }      
      }
    }
  }
  return(p)
}

draw_fat_tail<- function(p,fat_tail,nrows,list_dfs,color_guild,pos_tail_x,pos_tail_y,fattailjumphoriz,fattailjumpvert,fgap,inverse="no")
{
  ppos_tail_x <- pos_tail_x * fattailjumphoriz
  pos_tail_y <- (0.25+0.1*sqrt(nrows))*(list_dfs[[kcoremax]][1,]$y2+list_dfs[[kcoremax]][1,]$y1)/2
  ppos_tail_y <- pos_tail_y * fattailjumpvert
  if (nrow(fat_tail)>0)
  {
    plyy2 <- ifelse(inverse == "yes", list_dfs[[kcoremax]][1,]$y1-3*lado, list_dfs[[kcoremax]][1,]$y2-3*lado)
    v<- draw_tail(p,fat_tail,lado,color_guild,gen_sq_label(fat_tail$orph),
                  aspect_ratio,ppos_tail_x,ppos_tail_y,fgap,paintlinks,
                  lxx2 = list_dfs[[kcoremax]][1,]$x1, 
                  lyy2 = plyy2,
                  sqinverse = inverse, background = "no", psize = lsize_kcore1)
    p <- v["p"][[1]]
  }
  return(p)
}

handle_weirds <- function(p,weirds_a,weirds_b,str_guild_a,str_guild_b,lado,gap)
{
  weirds_a <- data.frame(c())
  weirds_b <- data.frame(c())
  if (exists("df_orph_a"))
    if (nrow(df_orph_a)>0)
     {
      weirds_a <-  df_orph_a[df_orph_a$repeated== "yes",]
      weirds_a <-  weirds_a[rev(order(weirds_a$orph,weirds_a$kcore)),]
      if (nrow(weirds_a)>0)
        weirds_a$drawn <- "no"
      }    
  if (exists("df_orph_b")) 
    if (nrow(df_orph_b)>0)
      {
      weirds_b <-  df_orph_b[df_orph_b$repeated== "yes",]
      weirds_b <-  weirds_b[rev(order(weirds_b$orph,weirds_b$kcore)),]
      if (nrow(weirds_b)>0)
        weirds_b$drawn <- "no"
      } 
    
  
  # Create empty df_chains data frame
  df_chains <- data.frame(x1 = numeric(0), x2 = numeric(0), y1 = numeric(0), y2 = numeric(0),
                          guild = character(0), orph = integer(0), partner = integer(0), 
                          kcorepartner = integer(0), xx2 = numeric(0), yy2 = numeric(0), stringsAsFactors = FALSE )
  
  if  (( ( nrow(weirds_a)+nrow(weirds_b) )>0)) {
    original_weirds_a <- weirds_a
    original_weirds_b <- weirds_b
    while (((nrow(weirds_a)+nrow(weirds_b))>0))
    {
      if (nrow(weirds_a)>0){
        k <- store_root_leaf(weirds_a, df_chains, str_guild_a, lado, gap, original_weirds_a, original_weirds_b)
        df_chains <- k["df_chains"][[1]]
        weirds_a <- k["weirds"][[1]]
      }
      if (nrow(weirds_b)>0){
        k <- store_root_leaf(weirds_b, df_chains, str_guild_b, lado, gap, original_weirds_a, original_weirds_b)
        df_chains <- k["df_chains"][[1]]
        weirds_b <- k["weirds"][[1]]
      }    
      if (nrow(weirds_a)>0){
        k <- store_branch_leaf(weirds_a, weirds_b, df_chains, str_guild_a, lado, gap, original_weirds_a, original_weirds_b)
        df_chains <- k["df_chains"][[1]]
        weirds_a <- k["weirds"][[1]]
        weirds_b <- k["weirds_opp"][[1]]
      }
      if (nrow(weirds_b)>0){
        k <- store_branch_leaf(weirds_b, weirds_a, df_chains, str_guild_b, lado, gap, original_weirds_a, original_weirds_b)
        df_chains <- k["df_chains"][[1]]
        weirds_b <- k["weirds"][[1]]
        weirds_a <- k["weirds_opp"][[1]]
      }
      # Now they may be some weirds of core 1 linked to core 1 that were not
      # stored in the previous procedure
      weirds_a <- weirds_a[weirds_a$drawn == "no",]
      weirds_b <- weirds_b[weirds_b$drawn == "no",]  
    }  
    p <- draw_weird_chains(p, df_chains, lado, paintlinks)
  }
  calc_vals <- list("p" = p, "df_chains" = df_chains) 
  return(calc_vals)
}

write_annotations <- function(p, network_name)
{
  p <- p+ ggtitle(sprintf("Network %s ", network_name))
  p <- p + coord_fixed(ratio=aspect_ratio) +theme_bw() + theme(panel.grid.minor.x = element_blank(),
                                                               panel.grid.minor.y = element_blank(),
                                                               panel.grid.major.x = element_blank(),
                                                               panel.grid.major.y = element_blank(),
                                                               axis.text.x = element_blank(),
                                                               axis.text.y = element_blank(),
                                                               axis.ticks.x=element_blank(),
                                                               axis.ticks.y=element_blank(),
                                                               axis.title.x = element_blank(),
                                                               axis.title.y = element_blank(),
                                                               plot.title = element_text(lineheight=.7, face="plain"))
  if (hide_plot_border)
    p <- p + theme(panel.border=element_blank())
  landmark_top <- 1.4*max(last_ytail_b[!is.na(last_ytail_b)],1.2*ymax)*rescale_plot_area[2]
  mlabel <- "."
  landmark_right <- (tot_width+2*hop_x)*rescale_plot_area[1]
  p <- draw_square(p,landmark_right,0,1,"transparent",0.5,"transparent",0,0,0,slabel="",aspect_ratio)
  p <- p +annotate(geom="text", x= landmark_right, y=0, label=mlabel, 
                   colour = "red", size=1, hjust = 0, vjust = 0, angle = 0,  
                   guide =FALSE)
  landmark_left <- min(last_xtail_a[[kcoremax]],last_xtail_b[[kcoremax]])-min(hop_x,0.2*min(last_xtail_a[[kcoremax]],last_xtail_b[[kcoremax]]))
  landmark_left <- min(landmark_left, pos_tail_x)*rescale_plot_area[1]
  p <- p +annotate(geom="text", x=landmark_left, y=0, label=mlabel, 
                   colour = "red", size=2, hjust = 0, vjust = 0, angle = 0,  
                   guide =FALSE)
  x_span <- landmark_right - landmark_left
  
  if (!(flip_results)){
    x_legend <- 0.8*landmark_right*(1+displace_legend[1])
    y_legend <- 0.8*landmark_top*(1+displace_legend[2])
  } else {
    x_legend <- 0.8*landmark_top*(1+displace_legend[2])
    y_legend <- 0.8*landmark_right*(1+displace_legend[1])
  }
  p <- p + annotate(geom="text", x=x_legend, 
                    y=y_legend, 
                    label=name_guild_a, 
                    colour = color_guild_a[1], size=lsize_legend, 
                    hjust = 0, vjust = 0, angle = 0,  
                    guide =FALSE)
  p <- p + annotate(geom="text", x=x_legend, 
                    y=y_legend, 
                    label= paste("            ",name_guild_b), 
                    colour = color_guild_b[1], size=lsize_legend, 
                    hjust = 0, vjust = 0, angle = 0,  
                    guide =FALSE)
  p <- p +annotate(geom="text", x=landmark_left, 
                   y = y_legend,
                   label="core 1", 
                   colour = "cornsilk3", size=labels_size, hjust = 0, vjust = 0, 
                   angle = 0,  
                   guide =FALSE)
  return(p)
}

handle_orphans <- function(vg)
{
  df_orph_a <- data.frame(c())
  df_orph_b <- data.frame(c())
  mtxlinks <- data.frame(get.edgelist(vg))
  names(mtxlinks) <- c("guild_a","guild_b")
  if (length(grep(str_guild_b,mtxlinks[1,1]))>0)
    names(mtxlinks) <- rev(names(mtxlinks))
  orphans_a <- df_cores$species_guild_a[[1]]
  orphans_b <- df_cores$species_guild_b[[1]]
  if (!is.null(orphans_a)) 
    if (!is.na(orphans_a[1]))
      df_orph_a <- find_orphans(mtxlinks,orphans_a,g,guild_a="yes")
  if (!is.null(orphans_b)) 
    if (!is.na(orphans_b[1]))
      df_orph_b <- find_orphans(mtxlinks,orphans_b,g,guild_a="no")
  calc_vals <- list("mtxlinks" = mtxlinks, "orphans_a" = orphans_a, 
                    "orphans_b" = orphans_b, "df_orph_a" = df_orph_a, "df_orph_b" = df_orph_b ) 
  return(calc_vals) 
}

draw_inner_orphans <- function(p)
{
  if (kcoremax >2)
  {
    for (kc in seq(from = kcoremax-1, to = 2))
    {
      
      if (exists("df_orph_a")){
        w <- draw_innercores_tails(p,kc,list_dfs_b,df_orph_a,color_guild_a, inverse="no")
        p <- w["p"][[1]]
        last_xtail_b[kc] <- w["lastx"][[1]]
        last_ytail_b[kc] <-w["lasty"][[1]] 
      }
      
      if (exists("df_orph_b")){
        w <- draw_innercores_tails(p,kc,list_dfs_a,df_orph_b,color_guild_b, inverse="yes")
        p <- w["p"][[1]]
        last_xtail_a[kc] <- w["lastx"][[1]]
        last_ytail_a[kc] <-w["lasty"][[1]]
      }
    }
  } else {                                  # orphans when kcoremax == 2
    kcore2tail_vertical_separation <- kcore2tail_vertical_separation + 2
    if (exists("df_orph_a")){
      w <- draw_innercores_tails(p,2,list_dfs_b,df_orph_a,color_guild_a, inverse="yes")
      p <- w["p"][[1]]
      last_xtail_b[2] <- w["lastx"][[1]]
      last_ytail_b[2] <-w["lasty"][[1]] 
    }
    
    if (exists("df_orph_b")){
      w <- draw_innercores_tails(p,2,list_dfs_a,df_orph_b,color_guild_b, inverse="no")
      p <- w["p"][[1]]
      last_xtail_a[2] <- w["lastx"][[1]]
      last_ytail_a[2] <- w["lasty"][[1]]
    }
  }
  return(p)
}

handle_fat_tails <- function(p)
{
  fat_tail_x <- min(last_xtail_a[[kcoremax]],last_xtail_b[[kcoremax]],list_dfs_a[[kcoremax]][1,]$x1,list_dfs_b[[kcoremax]][1,]$y2)
  max_b_kdegree <- list_dfs_b[[kcoremax]][which(list_dfs_b[[kcoremax]]$kdegree == max(list_dfs_b[[kcoremax]]$kdegree)),]$label
  if (exists("df_orph_a"))
    fat_tail_a <- df_orph_a[(df_orph_a$partner == max(max_b_kdegree)) & (df_orph_a$repeated == "no"),]
  if (!exists("fat_tail_a"))
    fat_tail_a <- data.frame(c())
  max_a_kdegree <- list_dfs_a[[kcoremax]][which(list_dfs_a[[kcoremax]]$kdegree == max(list_dfs_a[[kcoremax]]$kdegree)),]$label
  if (exists("df_orph_b"))
    fat_tail_b <- df_orph_b[(df_orph_b$partner == max(max_a_kdegree)) & (df_orph_b$repeated == "no"),]
  if (!exists("fat_tail_b"))
    fat_tail_b <- data.frame(c())
  fgap <- 0.7*hop_x
  pos_tail_x <- min(last_xtail_a[[kcoremax]],last_xtail_b[[kcoremax]],list_dfs_b[[kcoremax]][1,]$x1-fgap,list_dfs_a[[kcoremax]][1,]$x1-fgap)
  nrows_fat <- nrow(fat_tail_b)+nrow(fat_tail_a)
  if (exists("fat_tail_a")) 
    p <- draw_fat_tail(p,fat_tail_a,nrows_fat,list_dfs_b,color_guild_a[1],pos_tail_x,pos_tail_y,fattailjumphoriz[1],fattailjumpvert[1],fgap,inverse="yes")
  if (exists("fat_tail_b")) 
    p <- draw_fat_tail(p,fat_tail_b,nrows_fat,list_dfs_a,color_guild_b[1],pos_tail_x,pos_tail_y,fattailjumphoriz[2],fattailjumpvert[2],fgap,inverse="no")
  calc_vals <- list("p" = p, "pos_tail_x" = pos_tail_x) 
  return(calc_vals)
}

draw_all_ziggurats <- function(p)
{
  if (kcoremax > 2)
  {
    for (kc in seq(from = kcoremax-1, to = 2))
    {
      
      if (sum(df_cores$num_species_guild_a[kc],df_cores$num_species_guild_b[kc])>0)
      {
        pointer_x <- (kcoremax-kc)*hop_x
        pointer_y <- pointer_y - sign(pointer_y)*(1/(0.8*(kcoremax-2))*(kcoremax-kc))*strips_height
        
        print(paste("kcoreent",kc, "pointer_x", pointer_x))
        if (df_cores$num_species_guild_a[kc] > 0){
          despl_pointer_y <- displace_y_a[kc] * ymax
          if ((kc == 2) )
          {
            edge_core <- "yes"
            pointer_y <- max(4,species_in_core2_a)*height_y+abs(max(list_dfs_b[[kcoremax]]$y2))
          }
          else {
            edge_core <- "no"
            if (primerkcore)
              pointer_y <- ymax
            else if ((df_cores$num_species_guild_a[kc-1] > 5) & !(primerkcore)){
              pointer_y <- pointer_y - (0.8+0.1*(kcoremax-kc)/kcoremax)*strips_height
            }
          }
          print(paste("kcore",kc,"zig position", pointer_y, "ymax", ymax))
          pystep <- height_y
          if (kcoremax < 5)
            wzig <- (0.8+((kc)/kcoremax))* width_zig
          else 
            wzig <- 1.3*width_zig
          if (kc == 2)
            wzig <- wzig *min(2,(1+0.1*sqrt(max(df_cores$num_species_guild_a[kc],df_cores$num_species_guild_b[kc]))))
          zig <-  draw_individual_ziggurat(g, basex = pointer_x, widthx = wzig, 
                                basey = pointer_y + despl_pointer_y, ystep = pystep, strlabels = df_cores$species_guild_a[[kc]],
                                strguild = str_guild_a,
                                sizelabels = lsize_zig, colorb = color_guild_a, numboxes = df_cores[kc,]$num_species_guild_a, 
                                zinverse = "yes", edge = edge_core, grafo = p)
          p <- zig["p"][[1]]
          list_dfs_a[[kc]] <- zig["dr"][[1]]
          last_xtail_a[[kc]] <- max(list_dfs_a[[kc]]$x2)
          last_ytail_a[[kc]] <- -max(abs(list_dfs_a[[kc]]$y2))
          posic_zig_a <- -max(posic_zig_a, abs(min(list_dfs_a[[kc]]$y2)))    
          posic_zig <-  max(abs(posic_zig_a), abs(posic_zig_b))
          primerkcore <- FALSE
          x <- c(0)
          y <- c(ymax)
        }
        if (df_cores[kc,]$num_species_guild_b>0){
          despl_pointer_y <- displace_y_b[kc] * ymax
          if ((kc == 2))
          {
            edge_core <- "yes"
            pointer_y <- max(4,species_in_core2_b)*height_y+abs(max(list_dfs_a[[kcoremax]]$y2))
          }
          else {
            edge_core <- "no"
            if (primerkcore)
              pointer_y <- ymax
            else if ((df_cores$num_species_guild_b[kc-1]>5) & !(primerkcore)) {
              pointer_y <- pointer_y - 0.2*height_y*df_cores[kc,]$num_species_guild_b
            }
          }
          pystep <- height_y
          if (kcoremax < 5)
            wzig <- (0.8+(kc/kcoremax))* width_zig
          else 
            wzig <- 1.3*width_zig
          if (kc == 2)
            wzig <- wzig *min(2,(1+0.1*sqrt(max(df_cores$num_species_guild_a[kc],df_cores$num_species_guild_b[kc]))))
          zig <-  draw_individual_ziggurat(g, basex = pointer_x, widthx = wzig,
                                basey = pointer_y + despl_pointer_y,  ystep = pystep, strlabels = df_cores$species_guild_b[[kc]],
                                strguild = str_guild_b, sizelabels = lsize_zig,
                                colorb = color_guild_b, numboxes = df_cores[kc,]$num_species_guild_b, 
                                zinverse = "no", edge = edge_core, grafo = p)
          p <- zig["p"][[1]]
          list_dfs_b[[kc]]<- zig["dr"][[1]]
          last_xtail_b[[kc]] <- max(list_dfs_b[[kc]]$x2)
          last_ytail_b[[kc]] <- max(abs(list_dfs_b[[kc]]$y2))
          posic_zig_b <- max(posic_zig_b,max(list_dfs_b[[kc]]$y2))    
          posic_zig <-  max(abs(posic_zig_a), abs(posic_zig_b))
          primerkcore <- FALSE
        }
      }
    }
  }
  calc_vals <- list("p" = p, "posic_zig" = posic_zig, "list_dfs_a" = list_dfs_a, "list_dfs_b" = list_dfs_b,
                    "last_xtail_a" = last_xtail_a, "last_ytail_a" = last_ytail_a,
                    "last_xtail_b" = last_xtail_b, "last_ytail_b" = last_ytail_b) 
  return(calc_vals)
}

draw_maxcore <- function()
{
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
              color = list_dfs_a[[kcoremax]]$col_row, size=lsize_kcoremax, alpha = 1)
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
              color = list_dfs_b[[kcoremax]]$col_row, size=lsize_kcoremax)
  calc_vals <- list("p" = p, "basey" = basey, "topy" = topy, "topxa" = topxa, "topxb" = topxb,
                    "list_dfs_a" = list_dfs_a, "list_dfs_b" = list_dfs_b,
                    "last_xtail_a" = last_xtail_a, "last_ytail_a" = last_ytail_a,
                    "last_xtail_b" = last_xtail_b, "last_ytail_b" = last_ytail_b) 
  return(calc_vals)
}

draw_coremax_tails <- function(p)
{
  long_kcoremax_tail <- FALSE
  leftjump <- (1.2-0.02*nrow(list_dfs_a[[kcoremax]])) * hop_x 
  point_x <- list_dfs_a[[kcoremax]][nrow(list_dfs_a[[kcoremax]]),]$x2 - leftjump
  point_y <- posic_zig * 0.8
  if (exists("df_orph_a"))
    long_tail_a <- df_orph_a[(df_orph_a$kcore == kcoremax) & (df_orph_a$repeated == "no"),]
  if ((exists("long_tail_a")) & (kcoremax > 2))
  {
    if (length(long_tail_a)>5)
      long_kcoremax_tail <- TRUE
    v<-  draw_edge_tails(p,point_x,(point_y+height_y*aspect_ratio),kcoremax,long_tail_a,list_dfs_b,color_guild_a, inverse = "yes", 
                         vertical = "no", orientation = "South", revanddrop = "yes",
                         pbackground = "no", tspline = "vertical", joinchars=joinstr)
    p <- v["p"][[1]]
    last_xtail_b[kcoremax] <- v["lastx"][[1]]
    last_ytail_b[kcoremax] <- v["lasty"][[1]]
  }
  leftjump <- (1.2-0.02*nrow(list_dfs_b[[kcoremax]]))* hop_x
  point_x <- list_dfs_b[[kcoremax]][nrow(list_dfs_b[[kcoremax]]),]$x2 - leftjump
  point_y <- -1* posic_zig * 0.8
  if (exists("df_orph_b"))
    long_tail_b <- df_orph_b[(df_orph_b$kcore == kcoremax) & (df_orph_b$repeated == "no"),]
  if ( (exists("long_tail_b")) & (kcoremax > 2) ){
    if (nrow(long_tail_b)>5)
      long_kcoremax_tail <- TRUE
    v<-  draw_edge_tails(p,point_x,point_y,kcoremax,long_tail_b,list_dfs_a,color_guild_b, inverse = "no", 
                         vertical = "no", orientation = "North", revanddrop = "yes",
                         pbackground = "no", tspline = "vertical", joinchars=joinstr)
    p <- v["p"][[1]]
    last_xtail_a[kcoremax] <- v["lastx"][[1]]
    last_ytail_a[kcoremax] <- v["lasty"][[1]]
  }
  calc_vals <- list("p" = p, 
                    "last_xtail_a" = last_xtail_a, "last_ytail_a" = last_ytail_a,
                    "last_xtail_b" = last_xtail_b, "last_ytail_b" = last_ytail_b) 
  return(calc_vals)
}

display_plot <- function(p, printfile, flip, plwidth=14, plheight=11, ppi = 300)
{
  if (flip)
    p <- p + coord_flip()
  if (printfile){
    dir.create(plotsdir, showWarnings = FALSE)
    png(paste0("",plotsdir,"/",network_name,"_ziggurat.png"), width=(14*ppi), height=11*ppi, res=ppi)
  }
  print(p)
  if (printfile)
    dev.off()
}

read_and_analyze <- function(directorystr,network_file)
{
  str_guild_a <- "pl"
  str_guild_b <- "pol"
  name_guild_a <- "Plants"
  name_guild_b <- "Pollinators"
  network_name <- strsplit(network_file,".csv")[[1]][1]
  slabels <- c("Plant", "Pollinator")
  if (grepl("_SD_",network_name)){
    str_guild_b <- "disp"
    name_guild_b <- "Dispersers"
    }
  result_analysis <- analyze_network(network_file, directory = directorystr, guild_a = str_guild_a, 
                                     guild_b = str_guild_b, plot_graphs = TRUE)
  calc_vals <- list("result_analysis" = result_analysis, "str_guild_a" = str_guild_a, "str_guild_b" = str_guild_b,
                    "name_guild_a" = name_guild_a, "name_guild_b" = name_guild_b,
                    "network_name" = network_name) 
  return(calc_vals)
}

def_configuration <- function(paintlinks, displaylabelszig , print_to_file, plotsdir, flip_results, aspect_ratio,
                              alpha_level, color_guild_a, color_guild_b,
                              color_link, alpha_link, size_link, 
                              displace_y_b, displace_y_a, labels_size, lsize_kcoremax, lsize_zig, lsize_kcore1, 
                              lsize_legend, lsize_core_box,
                              height_box_y_expand, kcore2tail_vertical_separation,  kcore1tail_disttocore,
                              innertail_vertical_separation , horiz_kcoremax_tails_expand,
                              factor_hop_x, displace_legend, fattailjumphoriz, fattailjumpvert,
                              coremax_triangle_height_factor, coremax_triangle_width_factor, displace_outside_component,
                              outsiders_separation_expand, weirdskcore2_horizontal_dist_rootleaf_expand,
                              weirdskcore2_vertical_dist_rootleaf_expand , weirds_boxes_separation_count,
                              root_weird_expand,hide_plot_border,rescale_plot_area,kcore1weirds_leafs_vertical_separation)
{
  # GLOBAL CONFIGURATION PARAMETERS
  paintlinks <<- paintlinks
  displaylabelszig <<- displaylabelszig
  print_to_file <<- print_to_file
  plotsdir <<- plotsdir
  flip_results <<- flip_results
  alpha_level <<- alpha_level
  color_guild_a <<- color_guild_a
  color_guild_b <<- color_guild_b
  color_link <<- color_link
  alpha_link <<- alpha_link
  size_link <<- size_link
  displace_y_b <<- displace_y_b
  displace_y_a <<- displace_y_a
  aspect_ratio <<- aspect_ratio
  labels_size <<- labels_size
  lsize_kcoremax <<- lsize_kcoremax
  lsize_zig <<- lsize_zig
  lsize_kcore1 <<- lsize_kcore1
  lsize_legend <<- lsize_legend
  lsize_core_box <<- lsize_core_box
  height_box_y_expand <<- height_box_y_expand
  kcore2tail_vertical_separation <<- kcore2tail_vertical_separation                 # Vertical separation of orphan boxes linked to core 2 in number of heights_y  
  kcore1tail_disttocore <<- kcore1tail_disttocore                            # Horizontal & Vertical distances of edge/weird tails linked to core 1 North & South
  innertail_vertical_separation <<- innertail_vertical_separation                  # Vertical separation of orphan boxes linked to inner cores in number of heights_y  
  horiz_kcoremax_tails_expand <<- horiz_kcoremax_tails_expand                  # horizontal separation of edge tails connected to kcoremax
  factor_hop_x <<- factor_hop_x
  displace_legend <<- displace_legend
  fattailjumphoriz <<- fattailjumphoriz
  fattailjumpvert <<- fattailjumpvert
  coremax_triangle_height_factor <<- coremax_triangle_height_factor
  coremax_triangle_width_factor <<- coremax_triangle_width_factor
  displace_outside_component <<- displace_outside_component
  outsiders_separation_expand <<- outsiders_separation_expand
  weirdskcore2_horizontal_dist_rootleaf_expand <<- weirdskcore2_horizontal_dist_rootleaf_expand        # Controls the distance of weird root leaves to partner in core 2
  weirdskcore2_vertical_dist_rootleaf_expand <<- weirdskcore2_vertical_dist_rootleaf_expand
  weirds_boxes_separation_count <<- weirds_boxes_separation_count                  # Separation of leaves of a weird tail
  root_weird_expand <<- root_weird_expand
  hide_plot_border <<- hide_plot_border
  rescale_plot_area <<- rescale_plot_area
  kcore1weirds_leafs_vertical_separation <<- kcore1weirds_leafs_vertical_separation
}

init_working_values <- function()
{
  joinstr <<- " "
  max_position_y_text_core <<- 0
  rg <<- V(result_analysis$graph)
  g <<- rg[rg$kdistance != Inf]
  outsider <<- rg[rg$kdistance == Inf]
  outsiders_a <<- outsider$name[grep(str_guild_a,outsider$name)]
  outsiders_b <<- outsider$name[grep(str_guild_b,outsider$name)]
  nodes_guild_a <<- grep(str_guild_a,g$name)
  nodes_guild_b <<- grep(str_guild_b,g$name)
  ind_cores <<- rev(sort(unique(g$kcorenum)))
  kcoremax <<- max(ind_cores)
  palcores <<- colorRampPalette(c("gold","gold3"))
  corecols <<- palcores(kcoremax)
  species_guild_a <<- rep(NA,kcoremax)
  species_guild_b <<- rep(NA,kcoremax)
  num_species_guild_a <<- rep(NA,kcoremax)
  num_species_guild_b <<- rep(NA,kcoremax)
  last_xtail_a <<- rep(NA,kcoremax)
  last_ytail_a <<- rep(NA,kcoremax)
  last_xtail_b <<- rep(NA,kcoremax)
  last_ytail_b <<- rep(NA,kcoremax)
  df_cores <<- data.frame(species_guild_a, species_guild_b, num_species_guild_a, num_species_guild_b)
  list_dfs_a <<- list()
  list_dfs_b <<- list()
  df_cores$num_species_guild_a <<- 0
  df_cores$num_species_guild_b <<- 0
}



draw_ziggurat_plot <- function()
{
  for (i in ind_cores) {
    nodes_in_core_a <<- g[(g$guild == str_guild_a)&(g$kcorenum == i)]$name
    nodes_in_core_b <<- g[(g$guild == str_guild_b)&(g$kcorenum == i)]$name
    df_cores[i,]$species_guild_a <<- list(unlist(lapply(nodes_in_core_a, function(x) strsplit(x,str_guild_a)[[1]][[2]])))
    df_cores[i,]$num_species_guild_a <<- length(nodes_in_core_a)
    df_cores[i,]$species_guild_b <<- list(unlist(lapply(nodes_in_core_b, function(x) strsplit(x,str_guild_b)[[1]][[2]])))
    df_cores[i,]$num_species_guild_b <<- length(nodes_in_core_b)
  }
  max_species_cores <<- max(df_cores[kcoremax,]$num_species_guild_a,df_cores[kcoremax,]$num_species_guild_b)
  num_a_coremax <<- df_cores[kcoremax,]$num_species_guild_a
  base_width <<- 2000 
  ymax <<- 2*base_width/aspect_ratio 
  tot_width <<- ymax * (1+0.25*(kcoremax - 2))
  species_in_core2_a <<- sum(df_cores[2,]$num_species_guild_a)
  species_in_core2_b <<- sum(df_cores[2,]$num_species_guild_b)
  species_in_almond_a <<- sum(df_cores[2:(kcoremax-1),]$num_species_guild_a)
  species_in_almond_b <<- sum(df_cores[2:(kcoremax-1),]$num_species_guild_b)
  height_y <<- ymax/(1.3*max(species_in_almond_a,species_in_almond_b))
  maxincore2 <<- max(species_in_core2_a,species_in_core2_b)
  if (kcoremax < 4)
    if (species_in_core2_a+species_in_core2_b < 6)
      height_y <<- (0.08)*ymax
  yoffset <<- height_y*maxincore2*height_box_y_expand
  fmult <<- (ymax+yoffset)/ymax
  ymax <<- ymax + yoffset
  tot_width <<- tot_width*fmult
  height_y <<- height_y * fmult * height_box_y_expand
  yoffset <<- height_y*maxincore2
  ymax <<- ymax * (1+0.1*height_box_y_expand)
  for (i in seq(3,kcoremax-1)){
    displace_y_a[i] <<- displace_y_a[i] + species_in_core2_a*height_y/ymax
    displace_y_b[i] <<- displace_y_b[i] + species_in_core2_b*height_y/ymax
  }
  
  hop_x <<- factor_hop_x*(tot_width)/max(1,(kcoremax-2))
  lado <<- min(0.05*tot_width,height_y * aspect_ratio)
  basey <<- (0.1+0.1*length(df_cores[kcoremax,]$num_species_guild_a))*ymax
  wcormax <<- 1.2*hop_x*coremax_triangle_width_factor
  topxa <<- 0.65*hop_x
  basex <<- topxa - wcormax
  miny <<- ymax
  posic_zig <<- 0
  posic_zig_a <<- 0
  posic_zig_b <<- 0
  topy <<- 0.3*ymax+basey
  strips_height <<- 0.6*(ymax-yoffset)/max(1,(kcoremax-2))
  # Draw max core triangles
  f <- draw_maxcore()
  p <- f["p"][[1]]
  posic_zig <<- f["posic_zig"][[1]]
  list_dfs_a <<- f["list_dfs_a"][[1]]
  list_dfs_b <<- f["list_dfs_b"][[1]]
  last_xtail_a <<- f["last_xtail_a"][[1]]
  last_ytail_a <<- f["last_ytail_a"][[1]]
  last_xtail_b <<- f["last_xtail_b"][[1]]
  last_ytail_b <<- f["last_ytail_b"][[1]]
  topy <<- f["topy"][[1]]
  topxa <<- f["topxa"][[1]]
  topxb <<- f["topxb"][[1]]
  # Draw inner almond ziggurats
  pointer_x <<- max(topxa, topxb)+hop_x
  pointer_y <<- ymax+height_y*max(df_cores$num_species_guild_a[kcoremax-1],df_cores$num_species_guild_b[kcoremax-1])
  width_zig <<- 0.3*hop_x
  primerkcore <<- TRUE
  f <- draw_all_ziggurats(p)
  p <- f["p"][[1]]
  posic_zig <<- f["posic_zig"][[1]]
  list_dfs_a <<- f["list_dfs_a"][[1]]
  list_dfs_b <<- f["list_dfs_b"][[1]]
  last_xtail_a <<- f["last_xtail_a"][[1]]
  last_ytail_a <<- f["last_ytail_a"][[1]]
  last_xtail_b <<- f["last_xtail_b"][[1]]
  last_ytail_b <<- f["last_ytail_b"][[1]]
  # Draw core boxex
  for (i in seq(kcoremax,2))
    if ((length(list_dfs_a[[i]])+length(list_dfs_b[[i]]))>0){
      v <- draw_core_box(p, i)
      p <- v[["p"]]
      max_position_y_text_core <<- v[["max_position_y_text_core"]]
    }
  # Hanlde orphans, species outside the ziggurat
  f <- handle_orphans(result_analysis$graph)
  mtxlinks <<- f["mtxlinks"][[1]]
  orphans_a <<- f["orphans_a"][[1]]
  orphans_b <<- f["orphans_b"][[1]]
  df_orph_a <<- f["df_orph_a"][[1]]
  df_orph_b <<- f["df_orph_b"][[1]]
  # Species of core 1 linked to max core (except the most generalist)
  gap <<-  4*height_y
  f <- draw_coremax_tails(p)
  p <- f["p"][[1]]
  last_xtail_a <<- f["last_xtail_a"][[1]]
  last_ytail_a <<- f["last_ytail_a"][[1]]
  last_xtail_b <<- f["last_xtail_b"][[1]]
  last_ytail_b <<- f["last_ytail_b"][[1]]
  # Fat tails - nodes of core 1 linked to most generalist of opposite guild. Left side of panel
  z <- handle_fat_tails(p)
  p <- z["p"][[1]]
  pos_tail_x <<- z["pos_tail_x"][[1]]
  # Nodes of core 1 linked to species in cores kcoremax-1 to core 2.
  p <- draw_inner_orphans(p)
  # Draw inner links
  p <- draw_inner_links(p)
  # Weirds management
  v <- handle_weirds(p,weirds_a,weirds_b,str_guild_a,str_guild_b,lado,gap)
  p <- v["p"][[1]]
  df_chains <<- v["df_chains"][[1]]
  # Specied outside the giant componente
  p <- handle_outsiders(p,outsiders,df_chains)
  # Legend, title and final annotations
  p <- write_annotations(p,network_name)
  display_plot(p,print_to_file,flip_results)
}

ziggurat_graph <- function(datadir,filename,
                           paintlinks = TRUE, displaylabelszig = TRUE, print_to_file = FALSE, plotsdir ="plot_results", flip_results = FALSE, 
                           aspect_ratio = 1,
                           alpha_level = 0.2, color_guild_a = c("#4169E1","#00008B"), color_guild_b = c("#F08080","#FF0000"),
                           color_link = "slategray3", alpha_link = 0.2, size_link = 0.5, 
                           displace_y_b = rep(0,11),
                           displace_y_a = rep(0,11),      
                           labels_size = 3.5, lsize_kcoremax = 3.5, lsize_zig = 3, lsize_kcore1 = 2.5, lsize_legend = 4, lsize_core_box = 2.5,
                           height_box_y_expand = 1, kcore2tail_vertical_separation = 1,  kcore1tail_disttocore = c(1,1),
                           innertail_vertical_separation = 1, horiz_kcoremax_tails_expand = 1,
                           factor_hop_x = 1, displace_legend = c(0,0), fattailjumphoriz = c(1,1), fattailjumpvert = c(1,1),
                           coremax_triangle_height_factor = 1, coremax_triangle_width_factor = 1, displace_outside_component = c(1,1),
                           outsiders_separation_expand = 1, weirdskcore2_horizontal_dist_rootleaf_expand = 1,
                           weirdskcore2_vertical_dist_rootleaf_expand = 0, weirds_boxes_separation_count = 1,
                           root_weird_expand = c(1,1), hide_plot_border = TRUE, rescale_plot_area = c(1,1),
                           kcore1weirds_leafs_vertical_separation = 1
                           )
{
  f <- read_and_analyze(datadir,filename)
  result_analysis <<- f["result_analysis"][[1]]
  str_guild_a <<- f["str_guild_a"][[1]]
  str_guild_b <<- f["str_guild_b"][[1]]
  name_guild_a <<- f["name_guild_a"][[1]]
  name_guild_b <<- f["name_guild_b"][[1]]
  network_name <<- f["network_name"][[1]]
  def_configuration(paintlinks, displaylabelszig , print_to_file, plotsdir, flip_results, aspect_ratio,
                    alpha_level, color_guild_a, color_guild_b,
                    color_link, alpha_link, size_link, 
                    displace_y_b, displace_y_a, labels_size, lsize_kcoremax, lsize_zig, lsize_kcore1, 
                    lsize_legend, lsize_core_box,
                    height_box_y_expand, kcore2tail_vertical_separation,  kcore1tail_disttocore,
                    innertail_vertical_separation , horiz_kcoremax_tails_expand,
                    factor_hop_x, displace_legend, fattailjumphoriz, fattailjumpvert,
                    coremax_triangle_height_factor, coremax_triangle_width_factor, displace_outside_component,
                    outsiders_separation_expand, weirdskcore2_horizontal_dist_rootleaf_expand,
                    weirdskcore2_vertical_dist_rootleaf_expand , weirds_boxes_separation_count,
                    root_weird_expand, hide_plot_border, rescale_plot_area,kcore1weirds_leafs_vertical_separation
                    )
  init_working_values()
  draw_ziggurat_plot()
}

# ziggurat_graph("data/","M_PL_031.csv",displace_legend = c(0,0),outsiders_separation_expand = 0.5, aspect_ratio = 1, print_to_file = TRUE)
# end_time <- proc.time()
# print(end_time - init_time)