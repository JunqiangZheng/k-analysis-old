library(grid)
library(gridExtra)
library(lmtest)
library(stargazer)
library(kcorebip)


  load("results/datos_analisis.RData")
  resultdf <- resultdf[!is.na(resultdf$MeanKradius),]
  language_out = "EN" 
  
  
  modelp <- lm(MeanKradius ~ log(Species), data = resultdf)
  fitted_modelp <- data.frame(
    Species = resultdf$Species, MeanKradius = resultdf$MeanKradius,
    predict(modelp, interval = "confidence")
  )
 
  layer_linep <- geom_line(
    mapping = aes(x =Species, y = fit),
    data = fitted_modelp,
    color = "darkgrey"
  )
  
  textoreg1p <- sprintf("y = %0.2f ln(x) + %0.2f\n",summary(modelp)$coefficients[2, 1] ,summary(modelp)$coefficients[1, 1])
  textoreg2p <- sprintf("R^2 = %0.2f",summary(modelp)$r.squared)

  p <- ggplot(resultdf, aes(x=Species,y=MeanKradius),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=15, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(1,10,50,100,200,400,700,1000))+coord_trans(x="log") +
    #geom_smooth(method = "glm")+
    geom_text(data = resultdf,aes(x = 3, y= 3.5, 
                               label = paste0(textoreg1p,textoreg2p)
    ), color= "black", hjust= 0, size = 4) +        
    theme_bw() +  
    theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          legend.position = 'none',
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=15),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  p <- p + layer_linep
  if (language_out == "ES"){
    p <- p +  xlab("Número de especies\n") + ylab(expression(paste(bar(k)[radius],"\n")))
  } else
    p <- p + xlab("Number of species\n") +  ylab(expression(paste(bar(k)[radius],"\n")))
  
  
  
  modelq <- lm(MeanKdegree ~ log(Species), data = resultdf)
  fitted_modelq <- data.frame(
    Species = resultdf$Species, MeanKdegree = resultdf$MeanKdegree,
    predict(modelq, interval = "confidence")
  )
  
  layer_lineq <- geom_line(
    mapping = aes(x =Species, y = fit),
    data = fitted_modelq,
    color = "darkgrey"
  )
  
  textoreg1q <- sprintf("y = %0.2f ln(x) + %0.2f\n",summary(modelq)$coefficients[2, 1] ,summary(modelq)$coefficients[1, 1])
  textoreg2q <- sprintf("R^2 = %0.2f",summary(modelq)$r.squared)


  q <- ggplot(resultdf, aes(x=Species,y=MeanKdegree),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=15, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(1,10,50,100,200,400,700,1000))+coord_trans(x="log") +
    #geom_smooth(method = "glm")+
    geom_text(data = resultdf,aes(x = 5, y= 7.5, 
                                  label = paste0(textoreg1q,textoreg2q)
    ), color= "black", hjust= 0, size = 4) +  
    theme_bw() +  
    theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          legend.position = "right",
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=15),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  q <- q + layer_lineq
  if (language_out == "ES"){
    q <- q + xlab("Número de Interactions\n") +  ylab(expression(paste(bar(k)["degree"],"\n")))
  } else
    q <- q + xlab("Number of Interactions\n") + ylab(expression(paste(bar(k)["degree"],"\n")))
  

  modelr <- lm(MeanKradius ~ log(Interactions), data = resultdf)
  fitted_modelr <- data.frame(
    Interactions = resultdf$Interactions, MeanKradius = resultdf$MeanKradius,
    predict(modelr, interval = "confidence")
  )
  
  layer_liner <- geom_line(
    mapping = aes(x =Interactions, y = fit),
    data = fitted_modelr,
    color = "darkgrey"
  )
  
  textoreg1r <- sprintf("y = %0.2f ln(x) + %0.2f\n",summary(modelr)$coefficients[2, 1] ,summary(modelr)$coefficients[1, 1])
  textoreg2r <- sprintf("R^2 = %0.2f",summary(modelr)$r.squared)
  
  r <- ggplot(resultdf, aes(x=Interactions,y=MeanKradius),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=15, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(1,10,50,100,500,1000,3000))+coord_trans(x="log") +
    #geom_smooth(method = "glm")+
    geom_text(data = resultdf,aes(x = 5, y= 3.5, 
                                  label = paste0(textoreg1r,textoreg2r)
    ), color= "black", hjust= 0, size = 4) +  
    theme_bw() +  
    theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          legend.position = 'none',
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=15),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  r <- r + layer_liner
  if (language_out == "ES"){
    r <- r + xlab("Número de enlaces\n") + ylab(expression(paste(bar(k)[radius],"\n")))
  } else
    r <- r + xlab("Number of links\n") + ylab(expression(paste(bar(k)[radius],"\n")))
  
  models <- lm(MeanKdegree ~ log(Interactions), data = resultdf)
  fitted_models <- data.frame(
    Interactions = resultdf$Interactions, MeanKdegree = resultdf$MeanKdegree,
    predict(models, interval = "confidence")
  )
  
  layer_lines <- geom_line(
    mapping = aes(x =Interactions, y = fit),
    data = fitted_models,
    color = "darkgrey"
  )
  
  textoreg1s <- sprintf("y = %0.2f ln(x) + %0.2f\n",summary(models)$coefficients[2, 1] ,summary(models)$coefficients[1, 1])
  textoreg2s <- sprintf("R^2 = %0.2f",summary(models)$r.squared)

  s <- ggplot(resultdf, aes(x=Interactions,y=MeanKdegree),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=15, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(1,10,50,100,500,1000,3000))+coord_trans(x="log") +
    #geom_smooth(method = "glm")+
    geom_text(data = resultdf,aes(x = 10, y= 7.6, 
                                  label = paste0(textoreg1s,textoreg2s)
    ), color= "black", hjust= 0, size = 4) +  
    theme_bw() +  
    theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          legend.position = "right",
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=15),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  s <- s + layer_lines
  if (language_out == "ES") {
    s <- s + xlab("Número de enlaces\n") + ylab(expression(paste(bar(k)["degree"],"\n")))
  } else
    s <- s + xlab("Number of links\n") + ylab(expression(paste(bar(k)["degree"],"\n")))
  
  
  modelt <- lm(MeanKradius ~ MaxKcore, data = resultdf)
  fitted_modelt <- data.frame(
    MaxKcore = resultdf$MaxKcore, MeanKradius = resultdf$MeanKradius,
    predict(modelt, interval = "confidence")
  )
  
  layer_linet <- geom_line(
    mapping = aes(x =MaxKcore, y = fit),
    data = fitted_modelt,
    color = "darkgrey"
  )
  
  textoreg1t <- sprintf("y = %0.2f x + %0.2f\n",summary(modelt)$coefficients[2, 1] ,summary(modelt)$coefficients[1, 1])
  textoreg2t <- sprintf("R^2 = %0.2f",summary(modelt)$r.squared)
  
  
  t <- ggplot(resultdf, aes(x=MaxKcore,y=MeanKradius),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=25, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(2,3,4,5,6,7,8,9,10,11))+
    #geom_smooth(method = "glm")+
    geom_text(data = resultdf,aes(x = 7, y= 3.6, 
                                  label = paste0(textoreg1t,textoreg2t)
    ), color= "black", hjust= 0, size = 4) +  
    theme_bw() +  
    theme(#panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_blank(),
          legend.position = 'none',
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.key = element_blank(),
          axis.title.x = element_text(color="grey30", size=13),
          axis.title.y = element_text(color="grey30", size=15),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  t <- t + layer_linet
  if (language_out == "ES") {
    t <- t + xlab("Indice k máximo\n") + ylab(expression(paste(bar(k)[radius],"\n")))
  } else
    t <- t + xlab("Max k index\n") + ylab(expression(paste(bar(k)[radius],"\n")))
  

  modelu <- lm(MeanKdegree ~ MaxKcore, data = resultdf)
  fitted_modelu <- data.frame(
    MaxKcore = resultdf$MaxKcore, MeanKdegree = resultdf$MeanKdegree,
    predict(modelu, interval = "confidence")
  )
  
  layer_lineu <- geom_line(
    mapping = aes(x =MaxKcore, y = fit),
    data = fitted_modelu,
    color = "darkgrey"
  )
  
  textoreg1u <- sprintf("y = %0.2f x + %0.2f\n",summary(modelu)$coefficients[2, 1] ,summary(modelu)$coefficients[1, 1])
  textoreg2u <- sprintf("R^2 = %0.2f",summary(modelu)$r.squared)
  
  
  u <- ggplot(resultdf, aes(x=MaxKcore,y=MeanKdegree),legendTextFont=c(15, "bold.italic", "red"),
              addRegLine=TRUE, regLineColor="blue") +
    geom_point(aes(size=25, colour = factor(Type)), alpha = 0.5) + 
    scale_fill_manual(values=c("chocolate3", "cyan4"),name="Type") +
    scale_colour_manual(values=c("chocolate3", "cyan4")) +
    guides(colour = guide_legend(override.aes = list(shape = 20, size = 8)),
           size = FALSE)+
    #scale_colour_manual(values=c("chocolate3", "cyan4")) +
    scale_shape_identity()+ scale_x_continuous(breaks=c(2,3,4,5,6,7,8,9,10,11))+
    #geom_smooth(method = "glm")+
    geom_text(data = resultdf,aes(x = 3, y= 8, 
                                  label = paste0(textoreg1u,textoreg2u)
    ), color= "black", hjust= 0, size = 4) +  
    theme_bw() +  
    theme(#panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
      panel.grid.major.x = element_blank(),
      legend.position = 'right',
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size=12),
      legend.key = element_blank(),
      axis.title.x = element_text(color="grey30", size=13),
      axis.title.y = element_text(color="grey30", size=15),
      axis.text.x = element_text(face="bold", color="grey30", size=12),
      axis.text.y = element_text(face="bold", color="grey30", size=12)
    )
  u <- u + layer_lineu
  if (language_out == "ES") {
    u <- u + xlab("Indice k máximo\n") + ylab(expression(paste(bar(k)["degree"],"\n")))
  } else
    u <- u + xlab("Max k index\n") + ylab(expression(paste(bar(k)["degree"],"\n")))

ppi <- 300
png(paste0("graphs/tamanyo_kdegree_kradius_",language_out,".png"), width=(11*ppi), height=12*ppi, res=ppi)
grid.arrange(p,q,r,s,t,u,ncol=2, nrow=3, widths=c(0.45,0.55))
dev.off()

# ppi <- 300
# png("ESTATICA_MaxKcore_kdegree_kradius.png", width=(11*ppi), height=4*ppi, res=ppi)
# grid.arrange(t,u,ncol=2, nrow=1, widths=c(0.45,0.55))
# dev.off()



# 
# print("Shapiro test")
# print(shapiro.test(resid(model)))
# print("Durban Watson")
# print(dwtest(model, alternative="two.sided"))
#stargazer(model,style="qje")