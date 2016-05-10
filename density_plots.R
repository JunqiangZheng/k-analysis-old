library(kcorebip)
df_pl_010 <- read.csv("analysis_indiv/M_PL_010_analysis.csv")
df_pl_010$network <- "PL_010" 
df_sd_007 <- read.csv("analysis_indiv/M_SD_007_analysis.csv")
df_sd_007$network <- "SD_007"
df_pl_021 <- read.csv("analysis_indiv/M_PL_021_analysis.csv")
df_pl_021$network <- "PL_021"
data <- rbind(df_pl_010,df_sd_007,df_pl_021)
data$network <- as.factor(data$network)
levels(data$network)[levels(data$network)=="PL_010"] <- "PL 010"
levels(data$network)[levels(data$network)=="PL_021"] <- "PL 021"
levels(data$network)[levels(data$network)=="SD_007"] <- "SD 007"
names(data)[names(data)=="network"]  <- "Network"
g <- ggplot() + geom_density(aes(x=kdegree, color = Network, fill = Network), alpha = .3, 
                             data=data)+scale_x_log10() + ylab("Density") + xlab("k-degree")+
              theme_bw() +
              theme(panel.border = element_blank(),
                    legend.key = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.y = element_line(linetype = 2, color="slategray"),
                    panel.grid.major.x = element_line(linetype = 2, color="slategray"),
                    panel.border = element_blank(),
                    legend.title = element_text(size=14, face="bold"),
                    legend.text = element_text(size=12, face="bold"),
                    plot.title = element_text(lineheight=.8, face="bold"),
                    axis.text = element_text(face="bold", size=12),
                    axis.title.x = element_text(face="bold", size=14),
                    axis.title.y  = element_text(face="bold", size=14) )
ppi <- 300
png("density_plots.png", width=(6*ppi), height=4*ppi, res=ppi)
print(g)
dev.off()
  