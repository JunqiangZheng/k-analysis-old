library(grid)
library(gridExtra)
source("network-kanalysis.R")

  # Read the general results to query NODF, links, etc
  load("results/datos_analisis.RData")
  data_networks <- resultdf
  rm(resultdf)


  red <- "M_PL_006"
  pref <- "RND"
  load(paste0("results/",pref,"datos_analisis_",red,".RData"))
  resultdf <- resultdf[!is.na(resultdf$MeanKdistance),]

  data_conf <- data_networks[data_networks$Network == paste0(red,".csv"),]

  # Add original value

  nrowsdf <- nrow(resultdf)
  resultdf[nrowsdf+1,]$NODF <- data_conf$NODF
  resultdf[nrowsdf+1,]$MeanKdistance <- data_conf$MeanKradius

  
  model <- lm(log(MeanKdistance) ~ NODF, data = resultdf)
  fitted_model <- data.frame(
    NODF = resultdf$NODF, MeanKdistance = resultdf$MeanKdistance,
    predict(model, interval = "confidence")
  )
  
  layer_line <- geom_line(
    mapping = aes(x = NODF, y = exp(fit)),
    data = fitted_model,
    color = "darkorchid2"
  )
  
  layer_ribbon <- geom_ribbon(
    mapping = aes( ymin = exp(lwr), ymax = exp(upr)),
    data = fitted_model,
    alpha = 0.1
  )
  
  p <- ggplot(resultdf, aes(y=as.numeric(MeanKdistance),x=as.numeric(NODF)),legendTextFont=c(15, "bold.italic", "red")) +
        geom_point(aes(colour = 100*as.numeric(RemovedLinks)/as.numeric(Interactions[1])), alpha = 0.5) + 
        annotate(geom="text", x= data_conf$NODF, y = data_conf$MeanKradius, label="*", 
           colour = "blue", size=10, hjust = 0.5, vjust = 0.5, angle = 0,  
           guide =FALSE) +
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
  
 

  ppi <- 300
  png(paste0("ESTATICA_",red,"_corr_rewiring.png"), width=(6*ppi), height=4*ppi, res=ppi)
  #grid.arrange(h,g,ncol=2, nrow=1, widths=c(0.5,0.5))
  print(p)
  dev.off()