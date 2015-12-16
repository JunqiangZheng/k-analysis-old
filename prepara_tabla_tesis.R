library(grid)
library(gridExtra)
source("network-kanalysis.R")

  # Read the general results to query NODF, links, etc
  load("results/datos_analisis.RData")
  dropcols <- c("Number","Species","MaxKradius","RemovedLinks","Cscore")
  data_networks <- resultdf[,!(names(resultdf) %in% dropcols)]
  destructions <- read.csv("results/destructions.csv", sep=";")
  data_networks <- cbind(data_networks,"Dif. Areas" = destructions$MusRank - destructions$KshellKradiusKdegree)
  write.csv(data_networks,"tabla_tesis.csv")
  
 