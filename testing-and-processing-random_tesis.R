library(scales)
library(grid)
library(gridExtra)
source("network-kanalysis.R")

wipe_random <- function(red)
{
  directorystr <- "data/"
  result_analysis <- analyze_network(red, directory = directorystr, guild_a = "pl", guild_b = "pol", plot_graphs = FALSE)
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
        distances <- V(result_analysis$graph)$kradius
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
    save(resultdf, file=paste0('results_rnd/',pref,'datos_analisis_',unlist(strsplit(red,".csv")),'.RData'), compress=TRUE)  
  }
}

alldir <- TRUE

if (alldir) {
  p<- Sys.glob("data/M*.csv")
  listfiles <- gsub("data/","",p)  
  listfiles <- c("M_PL_044.csv","M_PL_045.csv",
                "M_PL_046.csv","M_PL_047.csv","M_PL_048.csv","M_PL_049.csv","M_PL_050.csv",
                "M_PL_051.csv","M_PL_052.csv","M_PL_053.csv","M_PL_054.csv","M_PL_055.csv",
                "M_PL_056.csv","M_PL_057.csv","M_PL_058.csv","M_PL_059.csv","M_SD_001.csv",
                "M_SD_002.csv","M_SD_003.csv","M_SD_004.csv","M_SD_005.csv","M_SD_006.csv",
                "M_SD_007.csv","M_SD_008.csv","M_SD_009.csv","M_SD_010.csv","M_SD_011.csv",
                "M_SD_012.csv","M_SD_013.csv","M_SD_014.csv","M_SD_015.csv","M_SD_016.csv",
                "M_SD_017.csv","M_SD_018.csv","M_SD_019.csv","M_SD_020.csv","M_SD_021.csv",
                "M_SD_022.csv","M_SD_023.csv","M_SD_024.csv","M_SD_025.csv","M_SD_026.csv",
                "M_SD_027.csv","M_SD_028.csv","M_SD_029.csv","M_SD_030.csv")
} else
  listfiles <- c("M_PL_012.csv")#c("M_PL_001.csv","M_PL_006.csv")
for (i in listfiles)
  wipe_random(i)
