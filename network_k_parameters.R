# This script launches the analysis of every network stored in the directory data
# anb saves the individual results in analysis_indiv

source("network-kanalysis.R")

directorystr <- "data/"

ficheros <- Sys.glob("data/M*.csv")
dir.create("analysis_indiv", showWarnings = FALSE)
for (j in ficheros)
{
  red <- strsplit(j,"/")[[1]][2]
  red_name <- strsplit(red,".csv")[[1]][1]
  sguild_a = "pl"
  sguild_b = "pol"
  slabels <- c("Plant", "Pollinator")
  if (grepl("_SD_",red)){
    sguild_b = "disp"
    slabels <- c("Plant", "Disperser")
  }
  print(red)
  
  result_analysis <- analyze_network(red, directory = "data/", guild_a = sguild_a, guild_b = sguild_b, plot_graphs = TRUE)
  numlinks <- result_analysis$links
  results_indiv <- data.frame(Species = c(), kradius = c(), kdegree = c())
  for (i in V(result_analysis$graph))
    results_indiv <- rbind(results_indiv,data.frame(Species =V(result_analysis$graph)[i]$name, kdistance = V(result_analysis$graph)[i]$kradius,
                                                   kdegree = V(result_analysis$graph)[i]$kdegree))
  write.csv(results_indiv,file=paste0("analysis_indiv/",red_name,"_analysis.csv"),row.names=FALSE)
}