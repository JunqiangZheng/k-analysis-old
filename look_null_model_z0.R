library(bipartite)
library(stringr)
library(kcorebip)

#data(Safariland)

vecnames <- c("Network","type","null_method","cycles","NODF","media_nodf","sd_nodf","z_nodf",
              "Modularity","media_modularity","sd_modularity","z_modularity",
              "MeanKradius","media_MeanKradius", "sd_MeanKradius", "z_MeanKradius", 
              "specradius", "media_specradius", "sd_specradius", "z_specradius" )

load("results/datos_analisis.RData")

analizatodo <- TRUE
analizatodo <- FALSE
intentos <- 1000

write_results <- TRUE

if (analizatodo) {
  tipo_de_red <- c("Binary")
  p<- Sys.glob("data/M*.csv")
  listfiles <- gsub("data/","",p) 
  listfiles <- resultdf[is.element(resultdf$MatrixClass,tipo_de_red),]$Network
  
#   listfiles <- c("M_SD_018.csv",
#                  "M_SD_019.csv","M_SD_020.csv",
#   "M_SD_021.csv","M_SD_022.csv","M_SD_023.csv","M_SD_024.csv","M_SD_025.csv","M_SD_026.csv","M_SD_027.csv","M_SD_028.csv","M_SD_029.csv")
#   

} else {
  tipo_de_red <- c("Binary")
  lfiles <- c("M_PL_012.csv")
}

zinit_time <- proc.time()

margin <- 0.05
count <- 1
for (name_red in lfiles)
{
  print(sprintf("Red %s",name_red))
  red <- strsplit(name_red,".csv")[[1]][1]
  lf <- Sys.glob(paste0("resultsnulls/",red,"*_dfindivs_*.RData"))
  listfiles <- gsub("resultsnulls/","",lf)
  nullmodels <- c("r2dtable","swap.web","vaznull","suffle.web","mgen")
  
  load(paste0("resultsnulls/",listfiles[1]))
  intentos <- as.integer(strsplit(strsplit(listfiles[[1]],"cycles_")[[1]],"_method")[[2]][1])
  modelo <- str_replace(strsplit(strsplit(strsplit(listfiles[[1]],"cycles_")[[1]],"_method")[[2]][2],".RData"),"_","")
  
  mean_nodf <- mean(dfindivs$NODF, na.rm = TRUE)
  sd_nodf <- sd(dfindivs$NODF, na.rm = TRUE)
  mean_avgkradius <- mean(dfindivs$MeanKradius, na.rm = TRUE)
  sd_avgkradius <- sd(dfindivs$MeanKradius, na.rm = TRUE)
  
  raw_net <- read.csv(paste0("data/",name_red),header=TRUE,stringsAsFactors=FALSE)
  rnames <- raw_net[,1]
  raw_matrix <- apply(as.matrix.noquote(raw_net[,seq(2,ncol(raw_net) )]),2,as.numeric)
  dimnames(raw_matrix)[[1]] <- rnames
 
  if (tipo_de_red == "Weighted")
    metodo <- 2
    #metodo <- 1
  else
    metodo <- 5
  index_row <- 1
  found <- FALSE
  while (!found)
  {
    o <- nullmodel(raw_matrix, N=1, method=metodo)
    write.csv(o,"datatemp/temp.csv")
    result_analysis <- analyze_network("temp.csv", directory = "datatemp/", guild_a = "Plant", guild_b = "Pollinator", plot_graphs = FALSE, only_NODF = TRUE)
    
    NODF <- result_analysis$nested_values["NODF"]
    Kradius <- result_analysis$meandist
    print(paste("NODF",NODF,"Kradius",Kradius,"max k core",result_analysis$max_core ))
    zNODF <- (NODF-mean_nodf)/sd_nodf
    zKradius <- (Kradius - mean_avgkradius)/sd_avgkradius
    print(sprintf("zNODF %.2f zKradius %.2f count %d",zNODF,zKradius,count))
    if ((abs(zNODF) < margin) & (abs(zKradius) < margin))
    {
      found <- TRUE
      nfile <- paste0("nullnetworks/null_avg_",name_red)
      write.csv(o,nfile)
    }
    count <- count + 1
  }
  
}
