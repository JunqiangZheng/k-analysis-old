library(ggplot2)
library(grid)
library(gridExtra)

paint_curve <- function(filename, ttile, lcolor)
{
  network <- read.table(filename, quote="\"")
  names(network) <- c("dest_ratio","giant_comp")
  p <- ggplot(network) + geom_area(aes(x  = dest_ratio, y = giant_comp),fill=lcolor, alpha=.5) +
       xlab("\nFracción de extinciones primarias") + ylab("Fracción de la componente gigante\n")+
       theme_bw()+ scale_x_continuous(expand = c(0,0), limits = c(0,1)) +
       scale_y_continuous(expand = c(0,0), limits = c(0, 1)) + 
       theme(panel.grid.major.y = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.major.x = element_line(size = 0.3, linetype = 3, color="grey80"),
          panel.grid.minor = element_blank(),
          axis.title.x = element_text(color="grey30", size=14),
          axis.title.y = element_text(color="grey30", size=14),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(face="bold", color="grey30", size=12),
          axis.text.y = element_text(face="bold", color="grey30", size=12)) +
    ggtitle(ttitle)
  return(p)
}

filename <- "M_SD_030_Diam_extin_MusRank_2col.txt"
ttitle <- "M_SD_030 MusRank\n" 
h <- paint_curve(filename,ttitle,"seagreen2")
filename <- "M_SD_030_Diam_extin_KshKrad_2col.txt"
ttitle <- "M_SD_030 Algoritmo basado en k-shell\n" 
g <- paint_curve(filename,ttitle,"violetred3")

ppi <- 300
png("ESTATICA_destruction_comparativa_dosredes.png", width=(12*ppi), height=5*ppi, res=ppi)
grid.arrange(h,g,ncol=2, nrow=1, widths=c(0.5,0.5))
dev.off()

