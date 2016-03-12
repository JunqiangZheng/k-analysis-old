library(stringr)

load("results/datos_analisis.RData")
vecnames <- c("area_NoOrder","area_Krad","area_KshKrad","area_MR","area_Krisk","area_KriskKdegree","area_Kdegree","area_Degree", "area_eigenc" )
rj_dunne <-  read.table(paste0("../juanma/results/dunnemethod/DIAM_EXTIN_ALL_dunnemethod.txt"), quote="\"", comment.char="")
names(rj_dunne) <- vecnames
rj_juanma <- read.table(paste0("../juanma/results/juanmamethod/DIAM_EXTIN_ALL_juanmamethod.txt"), quote="\"", comment.char="")
names(rj_juanma) <- vecnames

resultdf$diff_areas_MR <- rj_juanma$area_MR - rj_dunne$area_MR
resultdf$diff_areas_KdegreeMR <- rj_juanma$area_MR - rj_juanma$area_Kdegree