library(grid)
library(gridExtra)
library(ggplot2)
library(kcorebip)



languageEl<- "EN"

min_interactions <- 100
fcol <- ifelse(min_interactions > 1,"lightblue", "seagreen3" )
if (languageEl == "ES"){
  ytitle <- ifelse(min_interactions > 1,
                   paste("N?mero de redes con m?s de",min_interactions,"enlaces\n"), "N?mero de redes\n" )
  xtitle <- expression( paste("\nCorrelaci?n de ",bar(k)[radius]," y NODF (50% recableado)"))
  medtext <- "Mediana"
} else{
  ytitle <- ifelse(min_interactions > 1,
                   paste("Number of networks with more of",min_interactions,"links\n"), "Number of networks\n" )
  xtitle <- expression( paste("\nCorrelation of ",bar(k)[radius]," and NODF (50% rewired)"))
  medtext <- "Median"
}


# Read the general results to query NODF, links, etc
load("results/datos_analisis.RData")
data_networks <- resultdf
rm(resultdf)
#listofnets <- data_networks[which((data_networks$Species>40) & (data_networks$Species<=200)),]$Network
listofnets <- data_networks[(data_networks$MatrixClass == "Binary") & (data_networks$Interactions >= min_interactions),]$Network
data_networks$RndCorr <- NA
alpha_level <- 0.9
zscores_all <- read.csv("resultsnulls/zscores_all.csv")

calc_correlation <- function(red)
{
  pref <- "RND"

  data_conf <- data_networks[data_networks$Network == paste0(red,".csv"),]
  load(paste0("results_rnd/",pref,"datos_analisis_",red,"_numexper_10.RData"))
  resultdf <- resultdf[!is.na(resultdf$MeanKdistance),]
  return(cor(log(resultdf$MeanKdistance),resultdf$NODF))

#   modelp <- lm(log(MeanKdistance) ~ NODF, data = resultdf)
#   return(summary(modelp)$adj.r.squared)
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

interv = 0.05
mediana = median(corrdf$RndCorr)
datat <- data.frame(medianvalue = mediana)
histo_dist <- ggplot(corrdf, aes(x=RndCorr)) +
        ggtitle("")+ ylab(ytitle) +
        xlab(xtitle) +
        scale_x_continuous(expand = c(0,0),lim=c(-1,0.1),
                           breaks=seq(-1,0.4,by= 0.2) ) +
        scale_y_continuous(expand = c(0,0), limits=c(0,10), breaks=seq(0,10,by= 2)) +
        geom_histogram(binwidth = interv, fill = fcol,
                       color = "white",  alpha = alpha_level) +
        geom_vline(xintercept=mediana, linetype="solid", color = "violetred1") +
        geom_text(data = datat,aes(x = 0.97*medianvalue, y= 9,
        label = sprintf(paste0("\n",medtext,": %1.2f"),datat$medianvalue)
        ), color= "violetred1", hjust= 0, size = 5) +
        theme_bw() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
        theme(panel.border = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
        plot.title = element_text(lineheight=.8, face="bold"),
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.text.x = element_text(face="bold", color="grey30", size=13),
        axis.text.y = element_text(face="bold", color="grey30", size=13),
        #axis.line = element_line(colour = "black"),
        axis.title.x = element_blank()
        )

corrdf$Asimetria = abs(corrdf$Plants-corrdf$Pollinators)/corrdf$Interactions
scatter_size <- ggplot(corrdf, aes(x=Interactions,y=RndCorr)) +
  geom_point(aes(size=(5*corrdf$Asimetria),
                 ), fill = "blue", colour="blue", shape = 21, alpha = 0.4) +
  #scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
  #scale_colour_manual(values=c("chocolate3", "cyan4")) +
  scale_shape_identity()+scale_x_log10(breaks=seq(100,2100,by=500))+
  ggtitle("")+ ylab("Correlaci?n\n") +
  xlab("\nN?mero de especies de la red") +
  geom_vline(xintercept=100, linetype="dotted", color = "violetred1") +
  theme_bw() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))+
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
        plot.title = element_text(lineheight=.8, face="bold"),
        legend.text = element_text(size=14),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.text.x = element_text(face="bold", color="grey30", angle=45,hjust=1,size=12),
        axis.text.y = element_text(face="bold", color="grey30", size=12),
        #axis.line = element_line(colour = "black",size = 1, linetype = "solid"),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
        #axis.ticks.y = element_blank()
  )

ppi <- 300
png(paste0("graphs/histo_corr_rewiring_mininteractions",min_interactions,"_",languageEl,".png"), width=(8*ppi), height=5*ppi, res=ppi)
#grid.arrange(histo_dist,scatter_size,ncol=2, nrow=1, widths=c(0.6,0.4))
print(histo_dist)
dev.off()

ppi <- 300
png(paste0("graphs/asimetria_corr.png"), width=(8*ppi), height=3*ppi, res=ppi)
print(scatter_size)
dev.off()
write.csv(corrdf,"results_rnd/corrdf_data.csv")

