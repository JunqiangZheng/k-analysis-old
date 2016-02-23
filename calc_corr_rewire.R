library(grid)
library(gridExtra)
library(stringr)
source("network-kanalysis.R")


load("results/datos_analisis.RData")
resultdf <- resultdf[resultdf$MatrixClass == "Binary",]

auxdf <- data.frame(Network = c(), correlation = c())
idf <- data.frame(Network = NA, correlation = NA)

for (i in resultdf$Network)
{
  name_file <- paste0("results_rnd/RNDdatos_analisis_",strsplit(i,".csv")[[1]][1],"_numexper_10.RData")
  load(name_file)
  print(name_file)
  idf$Network <- i
  resultdf <- resultdf[!is.na(resultdf$MeanKdistance),]
  idf$correlation <- cor(log(resultdf$MeanKdistance),resultdf$NODF)
  auxdf <- rbind(auxdf,idf)
}
plot(histogram(auxdf$correlation))
