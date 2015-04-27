library(scales)
library(grid)
library(gridExtra)
source("network-kanalysis.R")

wipe_random <- function(red)
{
  directorystr <- "data/"
  result_analysis <- analyze_network(red, directory = directorystr, guild_a = "pl", guild_b = "pol", plot_graphs = TRUE)
  numlinks <- result_analysis$links
  vecnames <- c("Network","Number","Species","Plants","Pollinators","Interactions","MaxKcore","MeanKdegree","MeanKdistance","MaxKdistance","NODF","Cscore","RemovedLinks", "Type") #"wine","Cscore")
  resultdf <- data.frame(matrix(ncol = length(vecnames), nrow = 0))
  names(resultdf) <- vecnames
  
  
  analizatodo <- TRUE
  numexper <- 20
  wipedperc <- 0.1
  
  vnodf <- rep(0,numexper)
  vkdist <- rep(0,numexper)
  pref <- ""
#   if (!alldir)
#     numexper = 1
  if(analizatodo)
  {
    unlink(Sys.glob("datarnd/M*.csv"))
    indexrow <- 1

    for (e in 1:numexper)
    {  
      print(paste0("EXPERIMENTO",e))
      pref <- "RND"
      directorystr <- "datarnd/"
      wipelinks <- seq(1,round(numlinks*wipedperc))
      for (qlinks in wipelinks)
      {
        randomize_and_write(result_analysis$matrix,red, bypercentage = TRUE,directory ="datarnd/", rlinks = qlinks)
      }
      nfiles <- Sys.glob(paste0(directorystr,paste0(strsplit(red,"\\.")[[1]][1],"*.csv")))
      for (l in nfiles)
      {
        print(l)
        namefile <- strsplit(l,"/")
        namenetwork <- namefile[[1]][2]
        resultdf[indexrow,]$RemovedLinks <- strsplit(strsplit(namefile[[1]][2],"\\.")[[1]][1],"_rnd_")[[1]][2]
        result_analysis <- analyze_network(namenetwork, directory = directorystr, guild_a = "pl", guild_b = "pol", plot_graphs = FALSE)
        resultdf[indexrow,]$Network <- namenetwork
        resultdf[indexrow,]$Number <- strsplit(strsplit(namenetwork,".csv")[[1]],"_")[[1]][3]
        resultdf[indexrow,]$Number <- sprintf("%02d",as.integer(resultdf[indexrow,]$Number))
        if (grepl("_SD_",resultdf[indexrow,]$Network))
          resultdf[indexrow,]$Type <- "Disperser"
        else
          resultdf[indexrow,]$Type <- "Pollinator"
        resultdf[indexrow,]$Plants <- result_analysis$num_guild_a
        resultdf[indexrow,]$Pollinators <- result_analysis$num_guild_b
        resultdf[indexrow,]$Species <-resultdf[indexrow,]$Pollinators + resultdf[indexrow,]$Plants
        resultdf[indexrow,]$Interactions <- numlinks
        resultdf[indexrow,]$MaxKcore <- result_analysis$max_core
        distances <- V(result_analysis$graph)$kdistance
        if (length(distances[distances!=Inf])>0) 
          resultdf[indexrow,]$MaxKdistance <- max(distances[distances!=Inf])
        else {
          print("ALAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAARRRRRRRRRRRRRRRRRRRRRRMAAAAAAAAAAAAAA")
          resultdf[indexrow,]$MaxKdistance <- NA
        }
        if (is.na(resultdf[indexrow,]$MeanKdistance)){
          resultdf[indexrow,]$MeanKdistance <- result_analysis$meandist
        } else {
          resultdf[indexrow,]$MeanKdistance <- resultdf[indexrow,]$MeanKdistance + result_analysis$meandist
        }
        #resultdf$MeanKdistance[indexrow] <- resultdf$MeanKdistance[[indexrow]] + result_analysis$meandist
        resultdf[indexrow,]$MeanKdegree <- result_analysis$meankdegree
        if (is.na(resultdf[indexrow,]$NODF)){
          resultdf[indexrow,]$NODF <- result_analysis$nested_values["NODF"]
        } else  {
          resultdf[indexrow,]$NODF <- resultdf[indexrow,]$NODF + result_analysis$nested_values["NODF"]
        }
        #resultdf[indexrow,]$wine <- result_analysis$nested_values["wine"]
        resultdf[indexrow,]$Cscore <- result_analysis$nested_values["C.score"]
        indexrow <- indexrow +1 
      }
      
    }  
    save(resultdf, file=paste0('results/',pref,'datos_analisis_',unlist(strsplit(red,".csv")),'.RData'), compress=TRUE)  
  }
}

alldir <- FALSE

if (alldir) {
  p<- Sys.glob("data/M*.csv")
  listfiles <- gsub("data/","",p)  
} else
  listfiles <- c("M_PL_001.csv")
for (i in listfiles)
  wipe_random(i)
  