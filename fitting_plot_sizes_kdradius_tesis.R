library(grid)
library(gridExtra)
library(lmtest)
library(stargazer)

source("network-kanalysis.R")


  load("results/datos_analisis.RData")
  resultdf <- resultdf[!is.na(resultdf$MeanKradius),]
  
  
#   model <- lm(MeanKradius ~ Species, data = resultdf)
#   fitted_model <- data.frame(
#     Species = resultdf$Species, MeanKradius = resultdf$MeanKradius,
#     predict(model, interval = "confidence")
#   )
#   
#  
#   layer_line <- geom_line(
#     mapping = aes(x =Species, y = fit),
#     data = fitted_model,
#     color = "darkorchid2"
#   )
#   
  

  p <- ggplot(resultdf, aes(x=Species,y=MeanKradius),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=15, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    xlab("\nNúmero de especies") + ylab("K-radius medio\n") +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(1,10,50,100,200,400,700,1000))+coord_trans(x="log") +
    #geom_smooth(method = "glm")+
    theme_bw() +  
    theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          legend.position = 'none',
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=13),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  
  q <- ggplot(resultdf, aes(x=Species,y=MeanKdegree),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=15, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    xlab("\nNúmero de especies") + ylab("K-degree medio\n") +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(1,10,50,100,200,400,700,1000))+coord_trans(x="log") +
    #geom_smooth(method = "glm")+
    theme_bw() +  
    theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          legend.position = "right",
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=13),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  
  
  r <- ggplot(resultdf, aes(x=Interactions,y=MeanKradius),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=15, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    xlab("\nNúmero de enlaces") + ylab("K-radius medio\n") +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(1,10,50,100,500,1000,3000))+coord_trans(x="log") +
    #geom_smooth(method = "glm")+
    theme_bw() +  
    theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          legend.position = 'none',
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=13),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  
  s <- ggplot(resultdf, aes(x=Interactions,y=MeanKdegree),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=15, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    xlab("\nNúmero de enlaces") + ylab("K-degree medio\n") +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(1,10,50,100,500,1000,3000))+coord_trans(x="log") +
    #geom_smooth(method = "glm")+
    theme_bw() +  
    theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          legend.position = "right",
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=13),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )

  
  t <- ggplot(resultdf, aes(x=MaxKcore,y=MeanKradius),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=25, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    xlab("\nIndice k máximo") + ylab("K-radius medio\n") +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(2,3,4,5,6,7,8,9,10,11))+
    #geom_smooth(method = "glm")+
    theme_bw() +  
    theme(#panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_blank(),
          legend.position = 'none',
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=13),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  
  u <- ggplot(resultdf, aes(x=MaxKcore,y=MeanKdegree),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=25, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    xlab("\nIndice k máximo") + ylab("K-degree medio\n") +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(2,3,4,5,6,7,8,9,10,11))+
    #geom_smooth(method = "glm")+
    theme_bw() +  
    theme(#panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
      panel.grid.major.x = element_blank(),
      legend.position = 'right',
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size=12),
      legend.key = element_blank(),
      axis.title.x = element_text(color="grey30", size=13),
      axis.title.y = element_text(color="grey30", size=13),
      axis.text.x = element_text(face="bold", color="grey30", size=12),
      axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
    

ppi <- 300
png("ESTATICA_tamanyo_kdegree_kradius.png", width=(11*ppi), height=12*ppi, res=ppi)
grid.arrange(p,q,r,s,t,u,ncol=2, nrow=3, widths=c(0.45,0.55))
dev.off()

ppi <- 300
png("ESTATICA_MaxKcore_kdegree_kradius.png", width=(11*ppi), height=4*ppi, res=ppi)
grid.arrange(t,u,ncol=2, nrow=1, widths=c(0.45,0.55))
dev.off()



# 
# print("Shapiro test")
# print(shapiro.test(resid(model)))
# print("Durban Watson")
# print(dwtest(model, alternative="two.sided"))
#stargazer(model,style="qje")