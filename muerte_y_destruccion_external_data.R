# Destroys the animals of a network according to al ist of external data

library(kcorebip)
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
  return(etq,num)
}  


print_params <- function(g, gcsizse, verbose = TRUE)
{
  num_species_a <- sum(rowSums(result_analysis$matrix) > 0)
  num_species_b <- sum(colSums(result_analysis$matrix) > 0)

  if (verbose){
    print(paste("Species in giant component network-analysis:",gcsizse))
  }
  #return(length(radiuses))
}

custom_extinctions <- function(def, extlista, prefix, verbose = TRUE)
{
  gc<- giant.component(result_analysis$graph)
  size_giant_c <- length(V(gc))
  gcnames <- as.character(V(gc)$name)
  unlink("datapeli/*wipe*")
  if (sum(!is.element(def$species,gcnames)) > 0)
    def[!is.element(def$species,gcnames),]$giant_component <- FALSE
  cuenta <- 1
  for ( j in extlista)
  {
    i = which(def$species == j)
    # if (def$giant_component[i]){
    idspecies <- guild_and_number(as.character(def$species[i]))
    if (verbose)
      print(paste(idspecies[["guild"]],idspecies[["num"]],"removed"))
    if (idspecies[["guild"]] == "pl")
      result_analysis$matrix[,as.integer(idspecies[["num"]])] = 0
    else # (idspecies[["guild"]] == "pol")
      result_analysis$matrix[as.integer(idspecies[["num"]]),] = 0
    
#    } else {
#       if (i == nrow(def)){
#         print(sprintf("Network destroyed, key: %s. %d primary extinctions %.02f%% of initial network size",extkey,primary_extinctions,100*primary_extinctions/ini_size_giant_c))
#         return(primary_extinctions)
#       break()
#       } else {
#         i <- i+1
#         print(paste("voy por la fila ",i))
#         next
#       }
#     }
      
    primary_extinctions <- primary_extinctions + 1
    ntemp <- paste0(str_replace(red,".csv","")," minus_",cuenta," ",prefix,".csv")
    write.csv(result_analysis$matrix,paste0("datapeli/",ntemp))
    if (kcoremax > 0) {
      if (paint_zigs)
        result_analysis <- analyze_network(ntemp, directory = "datapeli/", guild_a = sguild_a, guild_b = sguild_b, plot_graphs = FALSE)
      else  
        result_analysis <- analyze_network_fast(ntemp, directory = "datapeli/", guild_a = sguild_a, guild_b = sguild_b)
      kcoremax <- result_analysis$max_core
      gc<- giant.component(result_analysis$graph)
      size_giant_c <- length(V(gc))

      gcnames <- as.character(V(gc)$name)
      if (sum(!is.element(def$species,gcnames)) > 0)
        def[!is.element(def$species,gcnames),]$giant_component <- FALSE
      print_params(result_analysis$graph, size_giant_c, verbose = verbose)
      if (size_giant_c <0){
        print(sprintf("Giant Component destroyed, key: %s. %d primary extinctions %.02f%% of initial network size",extkey,primary_extinctions,100*primary_extinctions/ini_size_giant_c))
        if (paint_zigs){
          ziggurat_graph("datapeli/",ntemp,plotsdir="datapeli/peli/",print_to_file = paint_to_file, paint_outsiders = pout) 
          Sys.sleep(1)
        }
        return(primary_extinctions)
        break()
      }
      if (paint_zigs){
        ziggurat_graph("datapeli/",ntemp,plotsdir="datapeli/peli/",print_to_file = paint_to_file, paint_outsiders = pout) 
        Sys.sleep(1)
      }
    }
    cuenta <- cuenta + 1
  }
}



read_sequence <- function(redname,method,longitud)
{
  fich <- paste0("../juanma/results/juanmamethod/",redname,"_DeadSequence_extin_",method,"_juanmamethod.txt")
  seqd <- read.csv(fich, header=FALSE, stringsAsFactors=FALSE)
  seqd$V1 <- str_replace(seqd$V1,"animal ","")
  for (i in 1:nrow(seqd))
    seqd$V1[i] <- strsplit(seqd$V1[i]," ")[[1]][1]
  return(as.numeric(seqd$V1[seq(1,longitud)]))
}

redname <- "M_PL_007"
ficheros <- c(paste0("data/",redname,".csv"))
paint_zigs <- TRUE
paint_to_file <- TRUE
verb <- TRUE
pout <- FALSE
dir.create("extinctions", showWarnings = FALSE)
for (fred in ficheros)
{

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
  result_analysis <- analyze_network(red, directory = "data/", guild_a = sguild_a, 
                                     guild_b = sguild_b, plot_graphs = FALSE, only_NODF = TRUE)
  numlinks <- result_analysis$links
  kcorenums_orig <- result_analysis$g_cores
  kcoredegree_orig <- V(result_analysis$graph)$kdegree
  kcorerisk_orig <- V(result_analysis$graph)$krisk
  red_degree <- igraph::degree(result_analysis$graph)
  eigc <- eigen(get.adjacency(result_analysis$graph))$values
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
  gc <- giant.component(result_analysis$graph)
  size_giant_c <- length(V(gc))
  ini_size_giant_c <- size_giant_c
  print_params(result_analysis$graph,size_giant_c,verbose = verb)

  if (paint_zigs){
    ziggurat_graph("data/",red,plotsdir="datapeli/peli/",print_to_file = paint_to_file, paint_outsiders = pout)
    Sys.sleep(3)
  }

numextinct <- 10
sMR <- read_sequence(redname,"MusRank",numextinct)
sKdeg <- read_sequence(redname,"Kdegree",numextinct)

spetxt <- "pol"
prefix <- "by MusRank"
extlista <- paste0(spetxt,sMR)
print(prefix)
pr <- custom_extinctions(df_index_extinction,extlista, prefix, verbose = verb)

prefix <- "by Kdegree"
extlista <- paste0(spetxt,sKdeg)
print("")
print(prefix)
pr <- custom_extinctions(df_index_extinction,extlista, prefix, verbose = verb)
}