library(igraph)
library(bipartite)
library(ggplot2)

read_network <- function(namenetwork, guild_a, guild_b, directory="")
{
  m <- read.csv(paste0(directory,namenetwork),header=TRUE,row.names=1)
  numplants <- ncol(m)
  numpols <- nrow(m)
  g <- graph.empty()
  for (i in 1:numplants){
    g <- g + vertices(paste0(guild_a,i),color="white")
  }
  for (i in 1:numpols){
    g <- g + vertices(paste0(guild_b,i),color="red")
  }
  for (i in 1:numpols){
    for (j in 1:numplants){
      if (m[i,j]!=0) {
        g <- g + edges(paste0(guild_b,i),paste0(guild_a,j))
      }
    }
  }
  calc_values <- list("graph" = g, "matrix" = m, "numpols" = numpols, "numplants" = numplants)
  return(calc_values)
}

randomize_and_write <- function(matrix, namenetwork, rlinks = 0,  directory = "", bypercentage = TRUE)
{
    if (bypercentage)
      filesuf <- paste0("_rnd_",rlinks)
    else
      filesuf <- paste0("_lnk_",rlinks)
    if (rlinks > 0)
    {
      links <- matrix == 1
      nolinks <- matrix == 0
      rows <- nrow(matrix)
      cols <- ncol(matrix)
      if (bypercentage) 
        extractions <- round(rlinks*sum(links)/100)
      else 
        extractions <- rlinks
      onestozeroes <- sample(which(links),extractions)
      zeroestoones <- sample(which(nolinks),extractions)
      for (i in onestozeroes){
        matrix[i] = 0
      }
      for (i in zeroestoones){
        matrix[i] = 1
      }
    }
    nfile <- paste0(directory,strsplit(namenetwork,"\\.")[[1]][1],filesuf,".csv")
    write.csv(matrix,nfile)
}
  
analyze_network <- function(namenetwork, directory="", guild_a = "pl", guild_b = "pol", plot_graphs = FALSE)
{
    nread <- read_network(namenetwork, directory = directory, guild_a = guild_a, guild_b = guild_b)  
    g <- as.undirected(nread$g)
    m <- nread$matrix
    numpols <- nread$numpols
    numplants <- nread$numplants
    edge_matrix <- get.edges(g, E(g))
    spaths_mat <- shortest.paths(g)
    g_cores <- graph.coreness(g)
    if (plot_graphs){
      plot(g, vertex.size=8, layout=layout.kamada.kawai)#layout=layout.circle), vertex.label=NA)
      hist(g_cores)
    }
    lcores <- unique(g_cores)
    max_core <- max(lcores)
    min_core <- min(lcores)
    p <- rep(NA, length(lcores))
    for (k in lcores)
    {
      p[k] <- list(names(g_cores[g_cores == k]))
    }
    # Find plants and plos of a core
     
    plants_k <- list(rep(NA,max_core))
    pols_k <- list(rep(NA,max_core))
    for (i in 1:max_core)
    { 
      auxincore <- c(p[[i]][grep(guild_a,as.character(unlist(p[i])))])
      if (length(auxincore)>0){
        plants_k[[i]] <- auxincore
      }
      else{
        plants_k[[i]] <- c(NA)
      }
      auxincore <- c(p[[i]][grep(guild_b,as.character(unlist(p[i])))])
      if (length(auxincore)>0){
        pols_k[[i]] <- auxincore
      }
      else{
        pols_k[[i]] <- c(NA)
      }
    }
    plants_maxcore <- p[[max_core]][grep(guild_a,as.character(unlist(p[max_core])))]
    pols_maxcore <- p[[max_core]][grep(guild_b,as.character(unlist(p[max_core])))]
    V(g)$kdistance <- NA
    V(g)$kcorenum <- NA
    V(g)$kdegree <- 0
    E(g)$weights <- 1
    for (i in 1:max_core)
    {
      lnod <- p[[i]]
      if (!is.na(lnod))
        for (k in lnod)
          V(g)[k]$kcorenum <- i
    }

    for (i in 1:numpols){
      namepol <- paste0("pol",i)
      kdistance <- 0
      kdistance_core <- mean(spaths_mat[namepol,][plants_k[[max_core]]])
      if (!is.na(kdistance_core)){
        kdistance <- kdistance + kdistance_core
      }
      V(g)[namepol]$kdistance <- kdistance
      }
    for (i in 1:numplants){
      nameplant <- paste0(guild_a,i)
      kdistance <- 0
      kdistance_core <- mean(spaths_mat[nameplant,][pols_k[[max_core]]])
      if (!is.na(kdistance_core)){
          kdistance <- kdistance + kdistance_core
      }
      V(g)[nameplant]$kdistance <- kdistance
    }
    meandist <- mean(V(g)$kdistance[V(g)$kdistance != Inf])
    print(paste("Mean distance",meandist))
    nested_values<- nested(as.matrix(m), "ALL")
    print(paste("NODF",nested_values["NODF"])) #,"wine",nested_values["wine"]))
    
    # kdegree computation

    for (l in 1:nrow(edge_matrix))
    {
      polvertex = edge_matrix[l,1]
      plantvertex = edge_matrix[l,2]
      V(g)[polvertex]$kdegree = V(g)[polvertex]$kdegree + 1/(1+V(g)[plantvertex]$kdistance)
      V(g)[plantvertex]$kdegree = V(g)[plantvertex]$kdegree + 1/(1+V(g)[polvertex]$kdistance)
    }
    meankdegree <- mean(V(g)$kdegree)
    calc_values <- list("graph" = g, "max_core" = max_core, "nested_values" = nested_values, "numplants" = numplants, 
                        "numpols" = numpols, "links" = length(V(g)), "meandist" = meandist, "meankdegree" = meankdegree, 
                        "spaths_mat" = spaths_mat, "matrix" = as.matrix(m))
    return(calc_values)
}

red <- "M_PL_029.csv"
result_analysis <- analyze_network(red, directory = "data/", guild_a = "pl", guild_b = "pol", plot_graphs = TRUE)

#abort()

numlinks <- result_analysis$links
vecnames <- c("Network","Plants","Pollinators","Interactions","MaxKcore","MeanKdegree","MeanKdistance","MaxKdistance","NODF","Cscore","RemovedLinks") #"wine","Cscore")
resultdf <- data.frame(matrix(ncol = length(vecnames), nrow = 0))
names(resultdf) <- vecnames

directorystr <- "data/"

#analizatodo <- TRUE
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
          resultdf[indexrow,]$Plants <- result_analysis$numplants
          resultdf[indexrow,]$Pollinators <- result_analysis$numpols
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