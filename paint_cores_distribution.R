library(grid)
library(gridExtra)
library(stargazer)
library(ggplot2)

source("network-kanalysis.R")


  load("results/datos_analisis.RData")
  resultdf <- resultdf[!is.na(resultdf$MeanKdistance),]
  
  alpha_level = 0.5
  histo_core <- ggplot(resultdf, aes(x=MaxKcore)) +
  scale_fill_manual(values=c("chocolate3", "cyan4")) +
  scale_x_continuous(lim=c(1.5,1+max(resultdf$MaxKcore) ), breaks=seq(2,11), labels=seq(2,11)) +
  geom_histogram(width = 0.8, aplha =alpha_level, binwidth=1, aes(fill=resultdf$Type), alpha = alpha_level) +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="gray90"),
        plot.title = element_text(lineheight=.8, face="bold"),        
        legend.text = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.text.x = element_text(face="bold", color="grey30", size=12),
        axis.text.y = element_text(face="bold", color="grey30", size=12),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle("")+ ylab("Number of networks\n") + xlab("\nMax Kcore")


histo_dist <- ggplot(resultdf, aes(x=MeanKdistance)) +
  scale_fill_manual(values=c("chocolate3", "cyan4")) +
  scale_x_continuous(lim=c(1,4)) +
  geom_histogram(binwidth=.25,width = 0.8, aplha =alpha_level, binwidth=1, aes(fill=resultdf$Type), alpha = alpha_level) +
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
        axis.text.x = element_text(face="bold", color="grey30", size=12),
        axis.text.y = element_text(face="bold", color="grey30", size=12),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle("")+ ylab("Number of networks\n") + xlab("\nAverage Kdistance")


  p <- ggplot(resultdf, aes(y=Interactions,x=MaxKcore),legendTextFont=c(15, "bold.italic", "red")) +
  geom_point(aes(size=5, colour = factor(Type)), alpha = 0.5) + 
  scale_x_continuous(lim=c(1.5,1+max(resultdf$MaxKcore) ), breaks=seq(2,11), labels=seq(2,11)) +
  xlab("Max Kcore\n") + ylab("\nLinks")+
  scale_colour_manual(values=c("chocolate3", "cyan4")) +
  scale_shape_identity()+
  guides(col = guide_legend(override.aes = list(shape = 1, size = 0)),
         size = FALSE)+
  scale_y_log10() +
  ggtitle(sprintf("Correlation log(Number of links) - Max Kcore: %.02f", cor(log(resultdf$Interactions),resultdf$MaxKcore)) ) +
  theme_bw() + 
  theme(panel.border = element_blank(),
        panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
        panel.grid.major.x = element_line(size = 0.05, linetype = 3, color="grey90"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.key = element_blank(),
        legend.position = 'none',
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.ticks.x = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.text.x = element_text(face="bold", color="grey30", size=12),
        axis.text.y = element_text(face="bold", color="grey30", size=12))

    qcorrtext <- sprintf("Correlation log(species) - Max Kcore: %.02f", cor(log(resultdf$Species),resultdf$MaxKcore))
    daux = data.frame(px=c(9),py=c(13)) 
    q <- ggplot(resultdf, aes(y=Species,x=MaxKcore),legendTextFont=c(15, "bold.italic", "red")) +
      geom_point(aes(size=5, colour = factor(Type)), alpha = 0.5) + 
      scale_x_continuous(lim=c(1.5,1+max(resultdf$MaxKcore) ), breaks=seq(2,11), labels=seq(2,11)) +
      xlab("\nMax Kcore") + ylab("Number of species\n")+
      scale_colour_manual(values=c("chocolate3", "cyan4")) +
      scale_shape_identity()+
      guides(col = guide_legend(override.aes = list(shape = 1, size = 0)),
             size = FALSE)+
      scale_y_log10() +
      annotate (geom="text",x =daux$px, y = daux$py,label = qcorrtext, size=4, face='bold', color="orchid", angle = 0)+
      theme_bw() + 
      theme(panel.border = element_blank(),
            panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.key = element_blank(),
            legend.position = 'none',
            axis.title.x = element_text(color="grey30", size=14),
            axis.title.y = element_text(color="grey30", size=14),
            axis.ticks.x = element_blank(),
            axis.ticks.y  = element_blank(),
            axis.text.x = element_text(face="bold", color="grey30", size=12),
            axis.text.y = element_text(face="bold", color="grey30", size=12))

    qcorrtext <- sprintf("Correlation log(links) - Max Kcore: %.02f", cor(log(resultdf$Interactions),resultdf$MaxKcore))
    daux = data.frame(px=c(8),py=c(14)) 
    r <- ggplot(resultdf, aes(y=Interactions,x=MaxKcore),legendTextFont=c(15, "bold.italic", "red")) +
      geom_point(aes(size=5, colour = factor(Type)), alpha = 0.5) + 
      scale_x_continuous(lim=c(1.5,1+max(resultdf$MaxKcore) ), breaks=seq(2,11), labels=seq(2,11)) +
      xlab("\nMax Kcore") + ylab("Number of links\n")+
      scale_colour_manual(values=c("chocolate3", "cyan4")) +
      scale_shape_identity()+
      guides(col = guide_legend(override.aes = list(shape = 1, size = 0)),
             size = FALSE)+
      scale_y_log10() +
      annotate (geom="text",x =daux$px, y = daux$py,label = qcorrtext, size=4, face='bold', color="orchid", angle = 0)+
      theme_bw() + 
      theme(panel.border = element_blank(),
            panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.key = element_blank(),
            legend.position = 'none',
            axis.ticks.x = element_blank(),
            axis.ticks.y  = element_blank(),
            axis.title.x = element_text(color="grey30", size=14),
            axis.title.y = element_text(color="grey30", size=14),
            axis.text.x = element_text(face="bold", color="grey30", size=12),
            axis.text.y = element_text(face="bold", color="grey30", size=12))

    qcorrtext <- sprintf("Correlation NODF - Max Kcore: %.02f", cor(log(resultdf$NODF),resultdf$MaxKcore))
    daux = data.frame(px=c(8),py=c(9)) 
    s <- ggplot(resultdf, aes(y=NODF,x=MaxKcore),legendTextFont=c(15, "bold.italic", "red")) +
      geom_point(aes(size=5, colour = factor(Type)), alpha = 0.5) + 
      scale_x_continuous(lim=c(1.5,1+max(resultdf$MaxKcore) ), breaks=seq(2,11), labels=seq(2,11)) +
      xlab("\nMax Kcore") + ylab("Number of links\n")+
      scale_colour_manual(values=c("chocolate3", "cyan4")) +
      scale_shape_identity()+
      guides(col = guide_legend(override.aes = list(shape = 1, size = 0)),
             size = FALSE)+
      #scale_y_log10() +
      annotate (geom="text",x =daux$px, y = daux$py,label = qcorrtext, size=4, face='bold', color="orchid", angle = 0)+
      theme_bw() + 
      theme(panel.border = element_blank(),
            panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
            panel.grid.major.x = element_line(size = 0.05, linetype = 3, color="grey90"),
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.key = element_blank(),
            legend.position = 'none',
            axis.ticks.x = element_blank(),
            axis.ticks.y  = element_blank(),
            axis.title.x = element_text(color="grey30", size=14),
            axis.title.y = element_text(color="grey30", size=14),
            axis.text.x = element_text(face="bold", color="grey30", size=12),
            axis.text.y = element_text(face="bold", color="grey30", size=12))

ppi <- 300
png("corr_kmagnitudes.png", width=(12*ppi), height=6*ppi, res=ppi)
grid.arrange(histo_core,histo_dist,q,r,ncol=2, nrow=2, widths=c(1/2,1/2))
dev.off()