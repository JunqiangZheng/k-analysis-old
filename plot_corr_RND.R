library(grid)
library(gridExtra)
library(ggplot2)
source("network-kanalysis.R")

# Read the general results to query NODF, links, etc
load("results/datos_analisis.RData")
#resultdf <- read.csv("results/results_all_todo.csv",sep=";")

data_networks <- resultdf
rm(resultdf)
listofnets <- data_networks[which((data_networks$Species>40) & (data_networks$Species<=200)),]$Network
data_networks$RndCorr <- NA
alpha_level <- 0.9

calc_correlation <- function(red)
{
  pref <- "RND"
  
  data_conf <- data_networks[data_networks$Network == paste0(red,".csv"),]
  load(paste0("results/",pref,"datos_analisis_",red,".RData"))
  resultdf <- resultdf[!is.na(resultdf$MeanKdistance),]
  
 
  # Add original value

#   nrowsdf <- nrow(resultdf)
#   resultdf[nrowsdf+1,]$NODF <- data_conf$NODF
#   resultdf[nrowsdf+1,]$MeanKdistance <- data_conf$MeanKdistance
#   resultdf[nrowsdf+1,]$RemovedLinks <- 0
  return(cor(resultdf$NODF,resultdf$MeanKdistance)) 
}
  
for (i in listofnets)
{
  red <-strsplit(i,".csv")[[1]][1]
  rcorr <- calc_correlation(red)
  data_networks[data_networks$Network == paste0(red,".csv"),]$RndCorr = rcorr
  print(paste(red,rcorr))
}
save(data_networks, file=paste0('results/data_networks.RData'), compress=TRUE)
corrdf <- data_networks[!is.na(data_networks$RndCorr),]

interv = 0.2
histo_dist <- ggplot(corrdf, aes(x=RndCorr)) +
        ggtitle("")+ ylab("") + 
        xlab("\nCorrelation coeff. Rewiring and Kdistance") +
        scale_x_continuous(lim=c(-1,0.4), 
                           breaks=seq(-1,0.2, by= interv) ) +
                           #labels=seq(-1,0.4, by= interv)) +
        geom_histogram(binwidth = interv, width = 0.8, fill = "lightblue", color = "white", alpha = alpha_level) +
        theme_bw() +
        theme(panel.border = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="gray90"),
        plot.title = element_text(lineheight=.8, face="bold"),        
        legend.text = element_text(size=12),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.text.x = element_text(face="bold", color="grey30", size=12, hjust=-.8),
        axis.text.y = element_text(face="bold", color="grey30", size=12),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank()
        )


p <- ggplot(corrdf) + geom_point(aes(x=NODF,y=RndCorr))+
  ggtitle("")+ ylab("") + 
  xlab("\nCorrelation coeff. Rewiring and Kdistance") +
  scale_x_continuous(lim=c(-1,0.4), 
                     breaks=seq(-1,0.2, by= interv) ) +
  #labels=seq(-1,0.4, by= interv)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="gray90"),
        plot.title = element_text(lineheight=.8, face="bold"),        
        legend.text = element_text(size=12),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.text.x = element_text(face="bold", color="grey30", size=12, hjust=-.8),
        axis.text.y = element_text(face="bold", color="grey30", size=12),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank()
  )

# ppi <- 300
# png("histo_corr_rewiring.png", width=(6*ppi), height=3*ppi, res=ppi)
# print(histo_dist)
# dev.off()
print(histo_dist)


