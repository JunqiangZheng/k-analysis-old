library(grid)
library(gridExtra)
library(lmtest)
library(stargazer)

source("network-kanalysis.R")


  load("results/datos_analisis.RData")
  resultdf <- resultdf[!is.na(resultdf$MeanKradius),]
  
  
  model <- lm(log(MeanKradius) ~ Modularity, data = resultdf)
  fitted_model <- data.frame(
    Modularity = resultdf$Modularity, MeanKradius = resultdf$MeanKradius,
    predict(model, interval = "confidence")
  )
  
 
  layer_line <- geom_line(
    mapping = aes(x = Modularity, y = exp(fit)),
    data = fitted_model,
    color = "darkorchid2"
  )
  
  layer_ribbon <- geom_ribbon(
    mapping = aes( ymin = exp(lwr), ymax = exp(upr)),
    data = fitted_model,
    alpha = 0.1
  )
  
  p <- ggplot(resultdf, aes(y=MeanKradius,x=Modularity),legendTextFont=c(15, "bold.italic", "red")) +
        geom_text(aes(size=70,angle=0,colour = factor(Type), label = Number), fontface="bold", alpha = 0.3)+
        ylab("Average Kradius\n") + xlab("\nModularity")+
  scale_colour_manual(values=c("chocolate3", "cyan4")) +
  scale_shape_identity()+
  guides(col = guide_legend(override.aes = list(shape = 1, size = 0)),
         size = FALSE)+
  theme_bw() + 
  theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
        panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.key = element_blank(),
        legend.position = 'none',
        axis.title.x = element_text(color="grey30", size=14),
        axis.title.y = element_text(color="grey30", size=14),
        axis.text.x = element_text(face="bold", color="grey30", size=12),
        axis.text.y = element_text(face="bold", color="grey30", size=12))
  

  r <- ggplot(resultdf, aes(y=MeanKradius,x=Modularity),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=log(Species), colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    xlab("\nModularity") + ylab("Average Kradius\n") +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ coord_trans(y="log") +
    #geom_smooth(method = "glm")+
    theme_bw() +  
    theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          legend.position = "right",
          axis.title.x = element_text(color="grey30", size=14),
          axis.title.y = element_text(color="grey30", size=14),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  
ppi <- 300
png("corr_figs_Modularity.png", width=(11*ppi), height=4*ppi, res=ppi)
grid.arrange(p,r+layer_line+layer_ribbon,ncol=2, nrow=1, widths=c(0.45,0.55))
dev.off()

print("Shapiro test")
print(shapiro.test(resid(model)))
print("Durban Watson")
print(dwtest(model, alternative="two.sided"))
#stargazer(model,style="qje")