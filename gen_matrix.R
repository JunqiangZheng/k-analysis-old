rm(list=ls())
source("matrix_graph.R")

ficheros <- Sys.glob("data/M*.csv")

for (j in ficheros)
{
  red <- strsplit(j,"/")[[1]][2]
  #red_name <- strsplit(red,".csv")[[1]][1]
  sguild_a = "pl"
  sguild_b = "pol"
  slabels <- c("Plant", "Pollinator")
  if (grepl("_SD_",red)){
    sguild_b = "disp"
    slabels <- c("Plant", "Disperser")
  }
  print(red) 
  matrix_graph(red)
}