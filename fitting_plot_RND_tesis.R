library(grid)
library(gridExtra)
source("network-kanalysis.R")

  # Read the general results to query NODF, links, etc
  load("results/datos_analisis.RData")
  data_networks <- resultdf
  rm(resultdf)
  
  red <- "M_PL_004"
  pref <- "RND"
  load(paste0("results_rnd/",pref,"datos_analisis_",red,".RData"))
  resultdf <- resultdf[!is.na(resultdf$MeanKdistance),]
  
  lf <- Sys.glob(paste0("resultsnulls/",red,"*_dfindivs_*.RData"))
  listfiles <- gsub("resultsnulls/","",lf)
  
  load(paste0("resultsnulls/",listfiles[1]))


  data_conf <- data_networks[data_networks$Network == paste0(red,".csv"),]

  # Add original value

  nrowsdf <- nrow(resultdf)
  resultdf <- rbind(resultdf,resultdf[1,])
  resultdf[nrowsdf+1,]$NODF <- data_conf$NODF
  resultdf[nrowsdf+1,]$MeanKdistance <- data_conf$MeanKradius
  resultdf[nrowsdf+1,]$RemovedLinks <- 0
  nrowsdf <- nrowsdf+1
  posorig <- nrowsdf
  
  # Add original value number of simulations-1 to build predictive model
  
  intentos <- as.integer(strsplit(strsplit(listfiles[[1]],"cycles_")[[1]],"_method")[[2]][1])
  mresultdf <- resultdf
  for (k in 1:intentos-1)
    mresultdf[posorig+k,] <- resultdf[posorig,]
  

  
  model <- lm(log(MeanKdistance) ~ NODF, data = mresultdf)
  fitted_model <- data.frame(
    NODF = mresultdf$NODF, MeanKdistance = mresultdf$MeanKdistance,
    predict(model, interval = "confidence")
  )
  
  layer_line <- geom_line(
    mapping = aes(x = NODF, y = exp(fit)),
    data = fitted_model,
    color = "darkgrey"
  )
  
  layer_ribbon <- geom_ribbon(
    mapping = aes( ymin = exp(lwr), ymax = exp(upr)),
    data = fitted_model,
    alpha = 0.1
  )
  
  resultdf <- resultdf[!is.na(resultdf$Species),]
  p <- ggplot(resultdf, aes(y=as.numeric(MeanKdistance),x=as.numeric(NODF)),legendTextFont=c(15, "bold.italic", "red")) +
        geom_point(aes(colour = 100*as.numeric(RemovedLinks)/as.numeric(Interactions[1])), alpha = 0.5) + 
#         annotate(geom="text", x= data_conf$NODF, y = data_conf$MeanKradius, label="*", 
#            colour = "blue", size=10, hjust = 0.5, vjust = 0.5, angle = 0) +
        ylab("K-radius medio\n") + xlab("\nNODF")+
        scale_colour_gradient(low="blue", high="red",name = "Recableado (%)")+
        coord_trans(y="log") +
        ggtitle(sprintf("Red %s Correlación: %.02f", red, cor(resultdf$MeanKdistance,resultdf$NODF))) +
        theme_bw() + 
        theme(panel.border = element_blank(),
              panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey"),
              panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey"),
              panel.grid.minor = element_blank(),
              legend.key = element_blank(),
              legend.position = 'right',
              axis.line = element_line(colour = "black"),
              axis.title.x = element_text(color="grey30", size=12),
              axis.title.y = element_text(color="grey30", size=12),
              axis.text.x = element_text(face="bold", color="grey30", size=11),
              axis.text.y = element_text(face="bold", color="grey30", size=11),
              plot.title = element_text(lineheight=.8, face="plain")
              )

tresultdf <- resultdf
tresultdf$alfa <- 0.5
tresultdf$psize <- 1.5
tresultdf$shape <- 16
tresultdf$psize[posorig] <- 4
offset <- nrow(resultdf)

overlap_nulls <- TRUE

if (overlap_nulls) {
  for (i in 1:nrow(dfindivs)){
     tresultdf[i+offset,] <- tresultdf[1,]
     tresultdf$RemovedLinks[i+offset] = 0
     tresultdf$NODF[i+offset] = dfindivs$NODF[i]
     tresultdf$MeanKdistance[i+offset] = dfindivs$MeanKradius[i]
     tresultdf$alfa[i+offset] = 0.1
     tresultdf$psize[i+offset] <- 0.8
     tresultdf$shape[i+offset] <- 3
  }
}

title_plot <- list()
title_plot["ES"] <- "Red %s Correlación: %.02f"
title_plot["EN"] <- "Network %s Correlation: %.02f"
text_legend <- list()
text_legend["ES"] <- "Recableado (%)"
text_legend["EN"] <- "Rewiring (%)"
title_Y <- list()
title_Y["ES"] <- "K-radius medio\n"
title_Y["EN"] <- "Mean k-radius\n"
language <- "ES"

overlap <- ggplot(tresultdf, aes(y=as.numeric(MeanKdistance),x=as.numeric(NODF)),legendTextFont=c(15, "bold.italic", "red")) +
  geom_point(aes(colour = 100*as.numeric(RemovedLinks)/as.numeric(Interactions[1])), 
             alpha = tresultdf$alfa, size = tresultdf$psize,
             shape = tresultdf$shape) + 
#   annotate(geom="text", x= data_conf$NODF, y = data_conf$MeanKradius, label="*", 
#            colour = "black", size=15, hjust = 0.5, vjust = 0.5, angle = 0) +
  ylab(title_Y[[language]]) + xlab("\nNODF")+
  scale_colour_gradient(low="blue", high="red",name = text_legend[[language]])+
  coord_trans(y="log") +
  ggtitle(sprintf(title_plot[[language]], red, cor(resultdf$MeanKdistance,resultdf$NODF))) +
  theme_bw() + 
  theme(panel.border = element_blank(),
#         = element_line(size = 0.1, linetype = 3, color="seagreen"),
#         panel.grid.major.x = element_line(size = 0.1, linetype = 3, color="seagreen"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),
        legend.position = 'right',
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(color="grey30", size=12),
        axis.title.y = element_text(color="grey30", size=12),
        axis.text.x = element_text(face="bold", color="grey30", size=11),
        axis.text.y = element_text(face="bold", color="grey30", size=11),
        plot.title = element_text(lineheight=.8, face="plain")
  )

  ppi <- 300
  png(paste0("results_rnd/ESTATICA_",red,"_corr_rewiring.png"), width=(6*ppi), height=4*ppi, res=ppi)

  print(overlap + layer_line)
  dev.off()
