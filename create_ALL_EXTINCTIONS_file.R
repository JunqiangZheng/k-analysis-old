alldir <- TRUE


if (alldir)
  p<- Sys.glob("extinctions/M_*.csv")


dftot <- data.frame(Network = c(), giant_component = c(), krisk = c(), degree = c(), kdegree = c(), eigen = c())
  
for (i in p){
  df <- read.csv(i)
  dftot <- rbind(dftot,df)
}

write.csv(dftot,"extinctions/ALL_EXTINCTIONS.csv")