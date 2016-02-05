library(grid)
library(gridExtra)
library(ggplot2)
source("network-kanalysis.R")

# Read the general results to query NODF, links, etc
load("results/datos_analisis.RData")
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

interv = 0.1
mediana = median(corrdf$RndCorr)
datat <- data.frame(medianvalue = mediana)
histo_dist <- ggplot(corrdf, aes(x=RndCorr)) +
        ggtitle("")+ ylab("Número de redes\n") + 
        xlab("\nCorrelación k-radius medio vs. NODF (10% recableado)") +
        scale_x_continuous(expand = c(0,0),lim=c(-1,0.4), 
                           breaks=seq(-1,0.4,by= 0.2) ) +
        scale_y_continuous(expand = c(0,0), limits=c(0,12)) +
        geom_histogram(binwidth = interv, width = 0.7, fill = "lightblue", 
                       color = "white",  alpha = alpha_level) +
        geom_vline(xintercept=mediana, linetype="solid", color = "violetred1") +
        geom_text(data = datat,aes(x = 0.98*medianvalue, y= 12, 
        label = sprintf("\nMediana: %1.2f",datat$medianvalue)
        ), color= "violetred1", hjust= 0, size = 3.15) +        
        theme_bw() +
        theme(panel.border = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
        plot.title = element_text(lineheight=.8, face="bold"),        
        axis.title.x = element_text(color="grey30", size=12),
        axis.title.y = element_text(color="grey30", size=12),
        axis.text.x = element_text(face="bold", color="grey30", size=10),
        axis.text.y = element_text(face="bold", color="grey30", size=10),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank()
        )

corrdf$Asimetria = abs(corrdf$Plants-corrdf$Pollinators)/corrdf$Species
scatter_size <- ggplot(corrdf, aes(x=Species,y=RndCorr)) + 
  geom_point(aes(size=(5*corrdf$Asimetria), 
                 fill = factor(Type)), colour="white", shape = 23, alpha = 0.4) + 
  scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
  scale_colour_manual(values=c("chocolate3", "cyan4")) +
  scale_shape_identity()+
  ggtitle("")+ ylab("Correlación\n") + 
  xlab("\nNúmero de especies de la red") +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
        plot.title = element_text(lineheight=.8, face="bold"),        
        legend.text = element_text(size=12),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(color="grey30", size=12),
        axis.title.y = element_text(color="grey30", size=12),
        axis.text.x = element_text(face="bold", color="grey30", size=10),
        axis.text.y = element_text(face="bold", color="grey30", size=10),
        axis.line = element_line(colour = "black"),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
        #axis.ticks.y = element_blank()
  )

ppi <- 300
png("ESTATICA_histo_corr_rewiring.png", width=(12*ppi), height=4*ppi, res=ppi)
grid.arrange(histo_dist,scatter_size,ncol=2, nrow=1, widths=c(0.6,0.4))
dev.off()

write.csv(corrdf,"corrdf_data.csv")

