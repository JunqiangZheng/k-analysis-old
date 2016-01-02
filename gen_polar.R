source("polar_graph.R")

ficheros <- Sys.glob("data/M*.csv")

gentodos <- FALSE

if (gentodos) {
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
    polar_graph(red,"data/",print_to_file=TRUE)
  }
} else
  #polar_graph("pl017-minus6plants.csv","datanetworks2015/",print_to_file=TRUE, lsize_title = 24, lsize_axis = 18, lsize_legend = 18, lsize_axis_title = 18, lsize_legend_title = 20)
  polar_graph("M_SD_004.csv","data/",print_to_file=TRUE, lsize_title = 24, lsize_axis = 18, lsize_legend = 18, lsize_axis_title = 18, lsize_legend_title = 20)
  #polar_graph("M_SD_004_minus_k4.csv","datanetworks2015/",print_to_file=FALSE, lsize_title = 24, lsize_axis = 18, lsize_legend = 18, lsize_axis_title = 18, lsize_legend_title = 20)
  #polar_graph("M_SD_007.csv","data/",print_to_file=TRUE, lsize_title = 24, lsize_axis = 18, lsize_legend = 18, lsize_axis_title = 18, lsize_legend_title = 20)
  