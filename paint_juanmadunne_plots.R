library(ggplot2)
library(grid)
library(gridExtra)

red <- "M_PL_002"

crit <- "MusRank"
metodo <- "dunnemethod"
if (metodo == "juanmamethod") {
  ytext <- "Remaining Giant Component (%)\n"
} else
  ytext <- "Secondary extinctions (%)\n"
juanma_criterio <- read.table(paste0("../juanma/results/",metodo,"/",red,"_Diam_extin_",crit,".txt"), quote="\"", comment.char="")
names(juanma_criterio) <- c("Primary","RemainingGC") 
p <- ggplot(data = juanma_criterio, aes(x = Primary*100, y = RemainingGC*100)) + geom_area(color = "violetred1",fill="violetred1",alpha=0.3) +
  theme_bw() + ylab(ytext)+xlab(paste0("\nPrimary extinctions (%) by ",crit))+ggtitle(red)+
  theme(panel.border = element_blank(),
        legend.key = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
        panel.grid.major.x = element_blank(), #,element_line(linetype = 2, color="slategray"),
        panel.border = element_blank(),
        legend.title = element_text(size=9, face="bold"),
        legend.text = element_text(size=9, face="bold"),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(lineheight=.6, face="bold"),
        axis.text = element_text(face="bold", size=10),
        axis.title.x = element_text(face="bold", size=10),
        axis.title.y  = element_text(face="bold", size=11) )

  crit <- "Kdegree"
  juanma_criterio <- read.table(paste0("../juanma/results/",metodo,"/",red,"_Diam_extin_",crit,".txt"), quote="\"", comment.char="")
  names(juanma_criterio) <- c("Primary","RemainingGC") 
  q <- ggplot(data = juanma_criterio, aes(x = Primary*100, y = RemainingGC*100)) + geom_area(color = "lightblue",fill="lightblue",alpha=0.3) +
    theme_bw() + ylab(ytext)+xlab(paste0("\nPrimary extinctions (%) by ",crit))+ggtitle(red)+
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
          panel.grid.major.x = element_blank(), #,element_line(linetype = 2, color="slategray"),
          panel.border = element_blank(),
          legend.title = element_text(size=9, face="bold"),
          legend.text = element_text(size=9, face="bold"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(lineheight=.6, face="bold"),
          axis.text = element_text(face="bold", size=10),
          axis.title.x = element_text(face="bold", size=10),
          axis.title.y  = element_text(face="bold", size=11) )
  
  crit <- "MusRank"
  metodo <- "juanmamethod"
  if (metodo == "juanmamethod") {
    ytext <- "Remaining Giant Component (%)\n"
  } else
    ytext <- "Secondary extinctions (%)\n"
  juanma_criterio <- read.table(paste0("../juanma/results/",metodo,"/",red,"_Diam_extin_",crit,".txt"), quote="\"", comment.char="")
  names(juanma_criterio) <- c("Primary","RemainingGC") 
  r <- ggplot(data = juanma_criterio, aes(x = Primary*100, y = RemainingGC*100)) + geom_area(color = "violetred1",fill="violetred1",alpha=0.3) +
    theme_bw() + ylab(ytext)+xlab(paste0("\nPrimary extinctions (%) by ",crit))+ggtitle(red)+
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
          panel.grid.major.x = element_blank(), #,element_line(linetype = 2, color="slategray"),
          panel.border = element_blank(),
          legend.title = element_text(size=9, face="bold"),
          legend.text = element_text(size=9, face="bold"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(lineheight=.6, face="bold"),
          axis.text = element_text(face="bold", size=10),
          axis.title.x = element_text(face="bold", size=10),
          axis.title.y  = element_text(face="bold", size=11) )
  
  crit <- "Kdegree"
  juanma_criterio <- read.table(paste0("../juanma/results/",metodo,"/",red,"_Diam_extin_",crit,".txt"), quote="\"", comment.char="")
  names(juanma_criterio) <- c("Primary","RemainingGC") 
  s <- ggplot(data = juanma_criterio, aes(x = Primary*100, y = RemainingGC*100)) + geom_area(color = "lightblue",fill="lightblue",alpha=0.3) +
    theme_bw() + ylab(ytext)+xlab(paste0("\nPrimary extinctions (%) by ",crit))+ggtitle(red)+
    theme(panel.border = element_blank(),
          legend.key = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
          panel.grid.major.x = element_blank(), #,element_line(linetype = 2, color="slategray"),
          panel.border = element_blank(),
          legend.title = element_text(size=9, face="bold"),
          legend.text = element_text(size=9, face="bold"),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(lineheight=.6, face="bold"),
          axis.text = element_text(face="bold", size=10),
          axis.title.x = element_text(face="bold", size=10),
          axis.title.y  = element_text(face="bold", size=11) )
  
  
  ppi <- 300
  png(paste0(red,"_",metodo,"_extinction_plot.png"), width=(8*ppi), height=8*ppi, res=ppi)
  grid.arrange(p,q,r,s,nrow=2,ncol=2)
  dev.off()