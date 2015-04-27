library(grid)
library(gridExtra)
source("network-kanalysis.R")

  # Read the general results to query NODF, links, etc
  load("results/datos_analisis.RData")
  data_networks <- resultdf
  rm(resultdf)


  red <- "M_PL_001"
  pref <- "RND"
  load(paste0("results/",pref,"datos_analisis_",red,".RData"))
  resultdf <- resultdf[!is.na(resultdf$MeanKdistance),]

  data_conf <- data_networks[data_networks$Network == paste0(red,".csv"),]

  # Add original value

  nrowsdf <- nrow(resultdf)
  resultdf[nrowsdf+1,]$NODF <- data_conf$NODF
  resultdf[nrowsdf+1,]$MeanKdistance <- data_conf$MeanKdistance
  resultdf[nrowsdf+1,]$RemovedLinks <- 0
  
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
  
  p <- ggplot(resultdf, aes(y=MeanKdistance,x=NODF),legendTextFont=c(15, "bold.italic", "red")) +
        geom_point(aes(colour = 100*as.numeric(RemovedLinks)/as.numeric(Interactions[1])), alpha = 0.5) + 
        annotate(geom="text", x= data_conf$NODF, y = data_conf$MeanKdistance, label="*", 
           colour = "blue", size=10, hjust = 0.5, vjust = 0.5, angle = 0,  
           guide =FALSE) +
        ylab("Average Kdistance\n") + xlab("\nNODF")+
        scale_colour_gradient(low="blue", high="red",name = "Rewired links(%)")+
        coord_trans(y="log") +
        ggtitle(sprintf("%s Correlation: %.02f", red, cor(resultdf$MeanKdistance,resultdf$NODF))) +
        theme_bw() + 
        theme(panel.border = element_blank(),
              panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
              panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
              panel.grid.minor = element_blank(),
              legend.key = element_blank(),
              legend.position = 'right',
              axis.title.x = element_text(color="grey30", size=14),
              axis.title.y = element_text(color="grey30", size=14),
              axis.text.x = element_text(face="bold", color="grey30", size=12),
              axis.text.y = element_text(face="bold", color="grey30", size=12),
              plot.title = element_text(lineheight=.5, face="plain")
              )
  
 

#print(p+layer_line+layer_ribbon)
print(p)
#grid.arrange(p,r+layer_line+layer_ribbon,ncol=2, nrow=1, widths=c(0.45,0.55))