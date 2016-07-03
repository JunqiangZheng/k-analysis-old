library(ggplot2)
library(grid)
library(gridExtra)

paint_curve <- function(filename, ttile, lcolor)
{
  network <- read.table(filename, quote="\"")
  names(network) <- c("dest_ratio","giant_comp")
  p <- ggplot(network, aes(x  = dest_ratio, y = giant_comp )) + geom_line(lwd = 1, color = lcolor) +
       xlab("\nPrimary destruction ratio") + ylab("Fraction of original giant component\n")+
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

filename <- "M_PL_010_Diam_extin_MusRank_2col.txt"
ttitle <- "M_PL_010 MusRank\n"
h <- paint_curve(filename,ttitle,"chocolate3")
filename <- "M_PL_010_Diam_extin_KshKrad_2col.txt"
ttitle <- "M_PL_010 K-shell ranking\n"
g <- paint_curve(filename,ttitle,"cyan4")

# ppi <- 300
# png("destruction_areas.png", width=(12*ppi), height=5*ppi, res=ppi)
# grid.arrange(h,g,ncol=2, nrow=1, widths=c(0.5,0.5))
# dev.off()

