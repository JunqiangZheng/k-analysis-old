p<- Sys.glob("data/M*.csv")
load("results/datos_analisis.RData")
resultdf$MatrixClass = "Weighted"
listfiles <- gsub("data/","",p) 
for (name_red in listfiles)
{
  print(name_red)
  raw_net <- read.csv(paste0("data/",name_red),header=TRUE,stringsAsFactors=FALSE)
  rnames <- raw_net[,1]
  raw_matrix <- apply(as.matrix.noquote(raw_net[,seq(2,ncol(raw_net) )]),2,as.numeric)
  dimnames(raw_matrix)[[1]] <- rnames
  if(sum(raw_matrix>1) == 0){
    print("Binary")
    resultdf[resultdf$Network == name_red,]$MatrixClass = "Binary"
  }
}
save(resultdf, file='results/datos_analisis.RData', compress=TRUE) 
