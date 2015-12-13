source("network-kanalysis.R")

load("results/datos_analisis.RData")

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
means <- data.frame(Red = NA, Media = NA, Modularity = NA)
means <- rbind(means,data.frame(Red ="PL 010", Media = mean(data[data$network == "PL 010",]$kdegree), Modularity = resultdf[resultdf$Network == "M_PL_010.csv",]$Modularity) )
means <- rbind(means,data.frame(Red ="PL 021", Media = mean(data[data$network == "PL 021",]$kdegree), Modularity = resultdf[resultdf$Network == "M_PL_021.csv",]$Modularity) )
means <- rbind(means,data.frame(Red ="SD 007", Media = mean(data[data$network == "SD 007",]$kdegree), Modularity = resultdf[resultdf$Network == "M_SD_007.csv",]$Modularity) )
names(data)[names(data)=="network"]  <- "Red"
g <- ggplot() + geom_density(aes(x=kdegree, color = Red, fill = Red), alpha = .3, 
                             data=data)+ scale_x_log10(expand = c(0,0), limits = c(0.1,100),breaks=c(0.1,0.5,1,2,5,10)) +  ylab("Densidad") + xlab("k-degree")+ 
                            scale_y_continuous(expand = c(0,0))+ ylab("Densidad\n") + xlab("\nK-degree")+
              geom_vline(xintercept=c(means[means$Red=="PL 010",]$Media), linetype="dotted", color = "violetred1", alpha= 0.9) +
              geom_text(data = means[means$Red=="PL 010",],aes(x = 1.15*Media, y= 1, 
                        label = sprintf("\nK-degree medio %1.2f \n Modularity %1.3f",means[means$Red=="PL 010",]$Media,means[means$Red=="PL 010",]$Modularity)
                        ), color= "violetred1", alpha= 0.9, hjust= 0, size = 3, angle = 90) +
              geom_vline(xintercept=c(means[means$Red=="SD 007",]$Media), linetype="dotted", color = "blue", alpha= 0.9) +
              geom_text(data = means[means$Red=="SD 007",],aes(x = 1.15*Media, y= 1.55, 
                                                   label = sprintf("\nK-degree medio %1.2f \n Modularity %1.3f",means[means$Red=="SD 007",]$Media,means[means$Red=="SD 007",]$Modularity)
               ), color= "blue", alpha= 0.7, hjust= 0, size = 3, angle = 90) +
              geom_vline(xintercept=c(means[means$Red=="PL 021",]$Media), linetype="dotted", color = "green3", alpha= 0.9) +
              geom_text(data = means[means$Red=="PL 021",],aes(x = 1.15*Media, y= 1.55, 
                                                   label = sprintf("\nK-degree medio %1.2f \n Modularity %1.3f",means[means$Red=="PL 021",]$Media,means[means$Red=="PL 021",]$Modularity)
               ), color= "green3", alpha= 0.9, hjust= 0, size = 3, angle = 90) +
              theme_bw() +
              theme(panel.border = element_blank(),
                    legend.key = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.y = element_line(linetype = 2, color="ivory3"),
                    panel.grid.major.x = element_blank(), #,element_line(linetype = 2, color="slategray"),
                    panel.border = element_blank(),
                    legend.title = element_text(size=10, face="bold"),
                    legend.text = element_text(size=8, face="bold"),
                    axis.line = element_line(colour = "black"),
                    plot.title = element_text(lineheight=.8, face="bold"),
                    axis.text = element_text(face="bold", size=10),
                    axis.title.x = element_text(face="bold", size=11),
                    axis.title.y  = element_text(face="bold", size=11) )
ppi <- 300
png("ESTATICA_density_plots.png", width=(6*ppi), height=3.7*ppi, res=ppi)
print(g)
dev.off()