p<- Sys.glob("data/M*.csv")
listfiles <- gsub("data/","",p)
redes<- unlist(strsplit(listfiles,".csv"))

metodos <- c("Degree","KshKrad","MusRank","Kdegree","Krisk")

results_dunne <- data.frame(Network=c(), Method = c(), Performance = c())

for (red in redes)
{
  for (i in metodos)
  {
    print(i)
    k <- read.csv(paste0("../juanma/weblife/",red,"_Diam_extin_",i,".txt"),sep=" ",stringsAsFactors = FALSE)
    data_read <- data.frame(Active = k[,1], Passive = k[,3])
    less_than_half <- data_read[data_read$Passive <= 0.5,]
    chosen_pair <- less_than_half[less_than_half$Passive == max(less_than_half$Passive),][1,]
    print(paste("Network:",red," Method:",i," Active (%):",100*chosen_pair$Active))
    results_aux <- data.frame(Network=red, Method = i, Performance = 100*chosen_pair$Active)
    results_dunne <- rbind(results_dunne,results_aux)
    
  }
}

results_dunne_min <- data.frame(Network=c(), Method = c(), Performance = c())
for (red in redes)
{
  min_df <- results_dunne[results_dunne$Network == red,]
  min_df <- min_df[min_df$Performance == min(min_df$Performance),]
  results_dunne_min <- rbind(results_dunne_min,min_df)
}

write.csv(results_dunne_min,"extinctions/results_dunne_half_min.csv")
