library(grid)
library(gridExtra)
library(stargazer)
library(ggplot2)

source("network-kanalysis.R")

  load("results/datos_analisis.RData")
  resultdf <- resultdf[!is.na(resultdf$MeanKradius),]
  
  alpha_level = 0.5
  histo_core <- ggplot(resultdf, aes(x=MaxKcore)) +
  scale_fill_manual(values=c("chocolate3", "cyan4")) +
  scale_x_continuous(expand = c(0,0), lim=c(1.5,1+max(resultdf$MaxKcore) ), breaks=seq(2,11), labels=seq(2,11)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_histogram(width = 0.5, binwidth=1, aes(fill=resultdf$Type), color = "white", alpha = alpha_level) +
  geom_vline(xintercept=median(resultdf$MaxKcore), color = "violetred") +
  geom_text(data = resultdf, aes(x = 4.2, y= 30, 
            label = sprintf("\nMediana %1.2f",median(resultdf$MaxKcore))
  ), color= "violetred", alpha= 0.9, hjust= 0, size = 4) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="gray"),
        plot.title = element_text(lineheight=.8, face="bold"),        
        legend.text = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.text.x = element_text(face="bold", color="grey30", size=11),
        axis.text.y = element_text(face="bold", color="grey30", size=11),
        axis.line = element_line(colour = "black"),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle("")+ ylab("Número de redes\n") + xlab("\nIndice k máximo")


histo_dist <- ggplot(resultdf, aes(x=MeanKradius)) +
  scale_fill_manual(values=c("chocolate3", "cyan4")) +
  scale_x_continuous(expand = c(0,0), breaks=seq(1,3.5, by=0.5)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_histogram(binwidth=0.25,width = 0.8,  aes(fill=resultdf$Type), color = "white", alpha = alpha_level) +
  geom_vline(xintercept=median(resultdf$MeanKradius), color = "violetred") +
  geom_text(data = resultdf, aes(x = 1.3, y= 19, 
                                 label = sprintf("\nMediana %1.2f",median(resultdf$MeanKradius))
  ), color= "violetred", alpha= 0.9, hjust= 0, size = 4) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="gray"),
        plot.title = element_text(lineheight=.8, face="bold"),        
        legend.text = element_text(size=12),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.text.x = element_text(face="bold", color="grey30", size=11),# hjust=-0.2),
        axis.text.y = element_text(face="bold", color="grey30", size=11),
        axis.line = element_line(colour = "black"),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle("")+ ylab("") + xlab("\nk-radius medio")


histo_deg <- ggplot(resultdf, aes(x=MeanKdegree)) +
  scale_fill_manual(values=c("chocolate3", "cyan4")) +
  #scale_x_continuous(lim=c(1,4)) +
  scale_x_continuous(expand = c(0,0), breaks=seq(0,8)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_histogram(binwidth=.5,width = 0.8, aes(fill=resultdf$Type),color="white", alpha = alpha_level) +
  geom_vline(xintercept=median(resultdf$MeanKdegree), color = "violetred") +
  geom_text(data = resultdf, aes(x = 1.15*median(resultdf$MeanKdegree), y= 25, 
                                                   label = sprintf("\nMediana %1.2f",median(resultdf$MeanKdegree))
  ), color= "violetred", alpha= 0.9, hjust= 0, size = 4) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="gray"),
        plot.title = element_text(lineheight=.8, face="bold"),        
        legend.text = element_text(size=12),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position=c(1,1.02),legend.justification=c(1,1),
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.text.x = element_text(face="bold", color="grey30", size=11),
        axis.text.y = element_text(face="bold", color="grey30", size=11),
        axis.line = element_line(colour = "black"),
        #axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle("")+ ylab("") + xlab("\nk-degree medio")



ppi <- 300
png("ESTATICA_hist_kmagnitudes.png", width=(12*ppi), height=4*ppi, res=ppi)
grid.arrange(histo_core,histo_dist,histo_deg,ncol=3, nrow=1, widths=c(1/3,1/3,1/3))
dev.off()