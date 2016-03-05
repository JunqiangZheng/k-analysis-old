# This script launches the analysis of every network stored in the directory data
# anb saves the individual results in analysis_indiv

source("network-kanalysis.R")
source("ziggurat_graph.R")
library(stringr)

directorystr <- "data/"

analyze_network_fast <- function(namenetwork, directory="", guild_a = "pl", guild_b = "pol")
{
  nread <- read_network(namenetwork, directory = directory, guild_astr = guild_a, guild_bstr = guild_b)
  g <- as.undirected(nread$g)
  g_cores <- graph.coreness(g)
  m <- nread$matrix
  #wtc <- walktrap.community(g)
  lcores <- unique(g_cores)
  max_core <- max(lcores)
  
  calc_values <- list("graph" = g, "matrix" = as.matrix(m), "max_core" = max_core)
  return(calc_values)
}


giant.component <- function(graph) { 
  cl <- clusters(graph) 
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))}

guild_and_number <- function(slabel)
{
  if (length(grep("pl",slabel)) == 1)
    etq <- "pl"
  else if (length(grep("pol",slabel)) == 1)
    etq <- "pol"
  else
    etq <- "disp"

  
  num <- strsplit(slabel,etq)[[1]][2]
  calc_vals <- list("guild" = etq, "num" = num) 
  return(calc_vals) 
  #return(etq,num)
}  

reorder_df <- function(df_network, bykey = "kdegree")
{
  if (sum(df_network$kradius == Inf) > 0)
    df_network[df_network$kradius == Inf,]$kradius <- 1000000
  if (bykey == "kdegree")
    df_network <- df_network[order(-df_network$kdegree),]
  if (bykey == "krisk")
    df_network <- df_network[order(-df_network$krisk),]
  if (bykey == "degree")
    df_network <- df_network[order(-df_network$degree),]
  if (bykey == "kshellkdegree")
    df_network <- df_network[order(-df_network$kcorenum,-df_network$degree),]
  if (bykey == "kshellkradius")
    df_network <- df_network[order(-df_network$kcorenum,df_network$kradius),]
  if (bykey == "kshellkradiuskdegree")
    df_network <- df_network[order(-df_network$kcorenum,df_network$kradius,-df_network$kdegree),]
  if (bykey == "kshell")
    df_network <- df_network[order(-df_network$kcorenum),]
  if (bykey == "eigenc")
    df_network <- df_network[order(-df_network$eigenc),]
  if (bykey == "kradius")
    df_network <- df_network[order(df_network$kradius),]
  return(df_network)
  
}

print_params <- function(g, gcsizse, verbose = TRUE)
{
  num_species_a <- sum(rowSums(result_analysis$matrix) > 0)
  num_species_b <- sum(colSums(result_analysis$matrix) > 0)
  if (verbose){
    print(paste("Species in giant component network-analysis:",gcsizse))
  }
}

dunnelike_extinctions <- function(def, extkey = "degree", verbose = TRUE, remguildstr = "pol", oppguild="pl")
{
  area <- 0
  base_mayor <- 1
 
  grafico_df <- data.frame(px=c(0),py=c(1))
  gc<- giant.component(result_analysis$graph)
  size_giant_c <- length(V(gc))
  init_giant_c <- size_giant_c
  gcnames <- as.character(V(gc)$name)
  if (sum(!is.element(def$species,gcnames)) > 0)
    def[!is.element(def$species,gcnames),]$giant_component <- FALSE
  for (k in def[!def$giant_component,]$species){
    idspecies <- guild_and_number(as.character(k))
    if (idspecies[["guild"]] == "pl")
      result_analysis$matrix[,as.integer(idspecies[["num"]])] = 0
    else # (idspecies[["guild"]] == "pol")
      result_analysis$matrix[as.integer(idspecies[["num"]]),] = 0
  }
  def <- reorder_df(def, bykey = extkey)
  def <- def[def$giant_component,]
  init_active <- sum(grepl(remguildstr,def$species))
  init_pasive <- sum(grepl(oppguild,def$species))
  ant_activos <- init_active
  stepx <- 1/(init_active+1)    
  unlink("datatemp/*wipe*")

  for ( i in 1:nrow(def))
  {
    if (!grepl(remguildstr,def$species[i]))
      next
    if (!def$giant_component[i])
      next
    # if (def$giant_component[i]){
    idspecies <- guild_and_number(as.character(def$species[i]))
      if (verbose)
        print(paste(idspecies[["guild"]],idspecies[["num"]],"removed"))
      if (idspecies[["guild"]] == "pl")
        result_analysis$matrix[,as.integer(idspecies[["num"]])] = 0
      else # (idspecies[["guild"]] == "pol")
        result_analysis$matrix[as.integer(idspecies[["num"]]),] = 0
    # } 

    primary_extinctions <- primary_extinctions + 1

    write.csv(result_analysis$matrix,paste0("datatemp/wipetemp_minus_",i,".csv"))
    if (kcoremax > 0) {
      if ((paint_zigs)  & (kcoremax >1))
        result_analysis <- analyze_network(paste0("wipetemp_minus_",i,".csv"), directory = "datatemp/", guild_a = sguild_a, guild_b = sguild_b, plot_graphs = FALSE)
      else  
        result_analysis <- analyze_network_fast(paste0("wipetemp_minus_",i,".csv"), directory = "datatemp/", guild_a = sguild_a, guild_b = sguild_b)
      kcoremax <- result_analysis$max_core
      if (verbose)
        print(paste("Kcoremax",kcoremax))
      gc<- giant.component(result_analysis$graph)
      size_giant_c <- length(V(gc))
      gcnames <- as.character(V(gc)$name)

      quedactivos <- sum(rowSums(result_analysis$matrix)>0)
      quedan <- sum(colSums(result_analysis$matrix)>0)
      
      area <- area + (base_mayor + (quedan/init_pasive))* 1/2 * (ant_activos-quedactivos)/init_active
      grafico_df[primary_extinctions+1,]$px <- grafico_df[primary_extinctions,]$px + (ant_activos-quedactivos)/init_active
      grafico_df[primary_extinctions+1,]$py <- quedan/init_pasive
      base_mayor <- (quedan/init_pasive)
      ant_activos <- quedactivos
      if (verbose){
        print(paste("quedan",quedan,"quedactivos",quedactivos,"area",area))
      }
      #print(gcnames)
      print_params(result_analysis$graph, size_giant_c, verbose = verbose)
      #if (size_giant_c <= 0.01*ini_size_giant_c){
#      if (size_giant_c == 0){
      if (quedactivos == 0){
        
        print(sprintf("Half Giant component destroyed, key: %s. %d primary extinctions %.02f%% of initial network size",extkey,primary_extinctions,100*primary_extinctions/ini_size_giant_c))
        print(paste("Area",area))
        if ((paint_zigs) & (kcoremax >1)) {
          ziggurat_graph("datatemp/",paste0("wipetemp_minus_",i,".csv"),plotsdir="peli/",print_to_file = paint_to_file, paint_outsiders = FALSE) 
          Sys.sleep(1)
        }
#         grafico_df[primary_extinctions+2,]$px <- 1.0
#         grafico_df[primary_extinctions+2,]$py <- 0.0
        write.csv(grafico_df,paste0("extinctions/figs/",str_replace(red,".csv","_"),extkey,".csv"),row.names=FALSE)
        return(area)
        break()
      }
      if ((paint_zigs)  & (kcoremax >1)){
        ziggurat_graph("datatemp/",paste0("wipetemp_minus_",i,".csv"),plotsdir="peli/",print_to_file = paint_to_file, paint_outsiders = FALSE) 
        Sys.sleep(1)
      }
    }
  }
}

alldir <- TRUE
alldir <- FALSE

killbyplants <- FALSE

if (alldir){
  ficheros <- Sys.glob("data/M*.csv")
} else
  ficheros <- c("data/M_PL_002.csv")
dir.create("extinctions", showWarnings = FALSE)
for (fred in ficheros)
{
  paint_zigs <- FALSE
  paint_to_file <- FALSE
  verb <- FALSE
  primary_extinctions <- 0
  red <- strsplit(fred,"data/")[[1]][2]
  red_name <- strsplit(red,".csv")[[1]][1]
  sguild_a = "pl"
  sguild_b = "pol"
  slabels <- c("Plant", "Pollinator")
  if (grepl("_SD_",red)){
    sguild_b = "disp"
    slabels <- c("Plant", "Disperser")
  }
  print(red)
  if (killbyplants) {
    result_analysis <- analyze_network(red, directory = "data/", guild_a = sguild_a, 
                                      guild_b = sguild_b, plot_graphs = FALSE, only_NODF = TRUE)
    write.csv(result_analysis$matrix,"datatemp/transp.csv")
    result_analysis <- analyze_network("transp.csv", directory = "datatemp/", guild_a = sguild_a, 
                                       guild_b = sguild_b, plot_graphs = FALSE, only_NODF = TRUE)
    numlinks <- result_analysis$links
  } else {
    result_analysis <- analyze_network(red, directory = "data/", guild_a = sguild_a, 
                                       guild_b = sguild_b, plot_graphs = FALSE, only_NODF = TRUE)
    numlinks <- result_analysis$links
  }
  
#   if (result_analysis$num_guild_a+result_analysis$num_guild_b>100)
#     next

  kcorenums_orig <- result_analysis$g_cores
  kcoredegree_orig <- V(result_analysis$graph)$kdegree
  kcorerisk_orig <- V(result_analysis$graph)$krisk

  red_degree <- igraph::degree(result_analysis$graph)
  eigc <- eigen(get.adjacency(result_analysis$graph))$values
  
  
#   for (i in 1:40){
#     kcorerisk_orig[result_analysis$num_guild_a+i] =  kcorerisk_orig[i]
#     kcoredegree_orig[result_analysis$num_guild_a+i] =  kcoredegree_orig[i]
#     red_degree[result_analysis$num_guild_a+i] =  red_degree[i]
#     eigc[result_analysis$num_guild_a+i] =  eigc[i]
#   }
#   
  df_index_extinction <- data.frame(species = c(), giant_component = c(), kcorenum = c(), kdegree = c(), degree = c(), 
                                    kradius = c(), krisk = c(), eigenc =c())
  for (j in 1:length(kcorenums_orig))
  {
    df_index_extinction <- rbind(df_index_extinction, data.frame(species = V(result_analysis$graph)$name[j], giant_component = TRUE,
                                                                 kcorenum = unlist(kcorenums_orig[j])[[1]], kdegree = kcoredegree_orig[j],
                                                                 degree = red_degree[j], kradius = V(result_analysis$graph)$kradius[j],
                                                                 krisk = kcorerisk_orig[j], eigenc = eigc[j]))
  }
  kcoremax <- max(result_analysis$g_cores)
  #print(paste("Kcoremax",kcoremax))
  gc <- giant.component(result_analysis$graph)
  size_giant_c <- length(V(gc))
  ini_size_giant_c <- size_giant_c
  print_params(result_analysis$graph,size_giant_c,verbose = verb)

  if ((paint_zigs)  & (kcoremax >1)){
    ziggurat_graph("data/",red,plotsdir="peli/",print_to_file = paint_to_file, paint_outsiders = FALSE)
    Sys.sleep(3)
  }

#dunnelike_extinctions(df_index_extinction, extkey = "kshellkradiuskdegree", verbose = FALSE)
#dunnelike_extinctions(df_index_extinction, extkey = "kshellkdegree", verbose = FALSE)

# pr_kshell <- dunnelike_extinctions(df_index_extinction, extkey = "kradius", verbose = verb, remguildstr = sguild_b, oppguild = sguild_a)
# pr_kshellkradius <- dunnelike_extinctions(df_index_extinction, extkey = "kshellkradius", verbose = verb, remguildstr = sguild_b, oppguild = sguild_a)
pr_kshell <- 1
pr_kshellkradius <- 1
  
pr_krisk <- dunnelike_extinctions(df_index_extinction, extkey = "krisk", verbose = verb, remguildstr = sguild_b, oppguild = sguild_a)
pr_degree <- dunnelike_extinctions(df_index_extinction, extkey = "degree", verbose = verb, remguildstr = sguild_b, oppguild = sguild_a)
pr_eigen <- dunnelike_extinctions(df_index_extinction, extkey = "eigenc", verbose = verb, remguildstr = sguild_b, oppguild = sguild_a)
#pr_eigen <- 1
pr_kdegree <- dunnelike_extinctions(df_index_extinction, extkey = "kdegree", verbose = verb, remguildstr = sguild_b, oppguild = sguild_a)

results_ext = data.frame(Network = red, giant_component = size_giant_c, kshell = pr_kshell, khsellkradius = pr_kshellkradius, krisk = pr_krisk,
                        degree = pr_degree, kdegree = pr_kdegree, eigenc = pr_eigen)

# results_ext <- read.csv(paste0("extinctions/",red_name,"-10.csv"))
# results_ext$krisk <- pr_krisk
if (alldir)
  write.csv(results_ext,file=paste0("extinctions/",red_name,"_dunnelike_byplants.csv"),row.names=FALSE)
}