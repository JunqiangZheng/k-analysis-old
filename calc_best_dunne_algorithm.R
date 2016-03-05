library(stringr)

juanma <- TRUE

killbyplants <- FALSE

if (killbyplants){
  appstr <- "_byplants"
} else
  appstr <- ""

# ignoreMR <- TRUE
# ignoreksh <- TRUE
# ignoreKrad <- TRUE
# ignoreKshKrad <- TRUE
ignoreMR <- FALSE
ignoreksh <- FALSE
ignoreKrad <- FALSE
ignoreKshKrad <- FALSE

if (juanma) {
  rj <-  read.table("D:/disco_usuario/javier/upm/doctorado/tesis/codigo/juanma/weblife/DIAM_EXTIN_ALL.txt", quote="\"", comment.char="")
  names(rj) <- c("area_ksh","area_Krad","area_KshKrad","area_MR","area_Krisk","area_Kdegree","area_Degree", "area_eigenc" )
  if (ignoreMR)
    rj$area_MR = 1.0
  if (ignoreksh)
    rj$area_Ksh = 1.0
  if (ignoreKrad)
    rj$area_Krad = 1.0
  if (ignoreKshKrad)
    rj$area_KshKrad = 1.0
  ficheros <- Sys.glob("data/M_SD_*.csv")
  redes <- str_replace(ficheros,"data/","")
  rj$Network <- redes
  rj$best <- apply(rj[,1:7],1,min)
  write.csv(rj,"extinctions/extinctions_dunne_juanma_area.csv")
  results_ext <- read.csv("extinctions/extinctions_dunne_juanma_area.csv")
  num_redes <- nrow(results_ext)
  print("Juanma algorithm")
  bkrisk <- sum(results_ext$area_Krisk <= results_ext$best)
  print(sprintf("krisk is the best for %d networks (%.2f%%)",bkrisk,100*bkrisk/num_redes))
  
  bksh <- sum(results_ext$area_Ksh <= results_ext$best)
  print(sprintf("kshell is the best for %d networks (%.2f%%)",bksh,100*bksh/num_redes))
  
  bkskr <- sum(results_ext$area_KshKrad <= results_ext$best)
  print(sprintf("KshKrad is the best for %d networks (%.2f%%)",bkskr,100*bkskr/num_redes))
  
  bkdegree <- sum(results_ext$area_Kdegree <= results_ext$best)
  print(sprintf("kdegree is the best for %d networks (%.2f%%)",bkdegree,100*bkdegree/num_redes))
  
  bdegree <- sum(results_ext$area_Degree <= results_ext$best)
  print(sprintf("degree is the best for %d networks (%.2f%%)",bdegree,100*bdegree/num_redes))
  
  bMR <- sum(results_ext$area_MR <= results_ext$best)
  print(sprintf("MusRank is the best for %d networks (%.2f%%)",bMR,100*bMR/num_redes))
  
  bkrad <- sum(results_ext$area_Krad <= results_ext$best)
  print(sprintf("krad is the best for %d networks (%.2f%%)",bkrad,100*bkrad/num_redes))
  
  beigenc <- sum(results_ext$area_eigenc <= results_ext$best)
  print(sprintf("eigenc is the best for %d networks (%.2f%%)",beigenc,100*beigenc/num_redes))
  
} else {
  ficheros <- Sys.glob(paste0("extinctions/M_*_dunnelike",appstr,".csv"))
  redes <- str_replace(ficheros,"extinctions/","")
  rj <- data.frame(Network = c(),giant_component =c(),krisk=c(),degree=c(),kdegree=c(),eigenc=c())
  for (j in ficheros){
    ad <- read.csv(j)
    rj <- rbind(rj,ad)
  }
  print("Dunne algorithm")
  rj$Network <- redes
  rj$best <- apply(rj[,3:8],1,min)
  write.csv(rj,"extinctions/extinctions_dunne_r_area.csv")
  results_ext <- read.csv("extinctions/extinctions_dunne_r_area.csv")
  num_redes <- nrow(results_ext)
  
  bkshell <- sum(results_ext$kshell <= results_ext$best)
  print(sprintf("kshell is the best for %d networks (%.2f%%)",bkshell,100*bkshell/num_redes))
  
  bkshellkradius <- sum(results_ext$bkshellkradius <= results_ext$best)
  print(sprintf("kshellkradius is the best for %d networks (%.2f%%)",bkshellkradius,100*bkshellkradius/num_redes))
  
  bkrisk <- sum(results_ext$krisk <= results_ext$best)
  print(sprintf("krisk is the best for %d networks (%.2f%%)",bkrisk,100*bkrisk/num_redes))
  
  bkdegree <- sum(results_ext$kdegree <= results_ext$best)
  print(sprintf("kdegree is the best for %d networks (%.2f%%)",bkdegree,100*bkdegree/num_redes))
  
  bdegree <- sum(results_ext$degree <= results_ext$best)
  print(sprintf("degree is the best for %d networks (%.2f%%)",bdegree,100*bdegree/num_redes))
  
  beigenc <- sum(results_ext$eigenc <= results_ext$best)
  print(sprintf("eigenc is the best for %d networks (%.2f%%)",beigenc,100*beigenc/num_redes))
  }

