library(grid)
library(gridExtra)
library(stringr)
#source("network-kanalysis.R")
libray(kcorebip)


load("results/datos_analisis.RData")
orresultdf <- resultdf[resultdf$MatrixClass == "Binary",]

auxdf <- data.frame(Network = c(), correlation = c(), beta=c())
idf <- data.frame(Network = NA, correlation = NA, beta = NA)

for (i in orresultdf$Network)
{
  name_file <- paste0("results_rnd/RNDdatos_analisis_",strsplit(i,".csv")[[1]][1],"_numexper_10.RData")
  load(name_file)
  print(name_file)
  idf$Network <- i
  resultdf <- resultdf[!is.na(resultdf$MeanKdistance),]
  idf$correlation <- cor(log(resultdf$MeanKdistance),resultdf$NODF)
  model <- lm(log(resultdf$MeanKdistance)~resultdf$NODF)
  idf$beta <- model$coefficients[2]
  auxdf <- rbind(auxdf,idf)
}
# plot(histogram(auxdf$correlation))
# plot(histogram(auxdf$beta))

# Correlation desctruction/beta

extinct <- read.csv("extinctions/ALL_EXTINCTIONS.csv")
bin_extinct <- extinct[is.element(extinct$Network,orresultdf$Network),]
auxipaint <- data.frame (dx = orresultdf$MeanKradius, dy = 100*bin_extinct$krisk/bin_extinct$giant_component)
p <- ggplot(data= auxipaint,aes(x=dx,y=dy))+geom_point()+
  ggtitle("Half destruction by krisk")+xlab("\n\nAvg Kradius")+ylab("Percentage of original GC removed")+
  theme_bw()+
  theme(axis.title.x = element_text(color="grey30", size=12),
        axis.title.y = element_text(color="grey30", size=12),
        axis.text.x = element_text(face="bold", color="grey30", size=10),
        axis.text.y = element_text(face="bold", color="grey30", size=10)
        )
modelkrisk <- lm(100*bin_extinct$krisk/bin_extinct$giant_component ~ orresultdf$MeanKradius)
cor(bin_extinct$degree/bin_extinct$giant_component , orresultdf$MeanKradius)
