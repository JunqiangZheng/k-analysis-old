source("polar_graph.R")

gen_html_file <- function(red,description,nextnet,prevnet)
{
  print(prevnet)
  texto <- paste0("<html><body align='left'>",
                  "<table width = '1000'>
                  <tr><th colspan=2 align=left>",
                  "Network ",red,"&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
  if (!is.na(prevnet))
    texto <- paste0(texto,"<a href=\'",prevnet,".html\'><&nbsp;Prev</a>&nbsp;&nbsp;")
  texto<- paste0(texto,"<a href='index.html'>Back to index</a>")
  if (!is.na(nextnet))
    texto <- paste0(texto,"&nbsp;&nbsp;<a href=\'",nextnet,".html\'>Next&nbsp;></a><br><br>")
  texto <- paste0(texto,"Reference ",description$Reference,"<br>",
                  "Species ",description$Species,"<br>",
                  "Links ",description$Interactions,"<br>",
                  "</th></tr>
                  <tr><td>
                  <img src='../matrix/",red,"_matrix.png'  width = '800'><p align=center>
                  <a href = '../matrix/",red,"_matrix.png' target=_BLANK>Click to enlarge</a></p>
                  </td><td>
                  <img src='../knot/", red,"_KM.jpg' ><p align=center>
                  <a href = '../knot/",red,"_KM.jpg' style='image-orientation: 90deg;' target=_BLANK>Click to enlarge</a></p>
                  </td></tr>
                  </table><p><center>")

  texto <- paste0(texto,"</body></html>")
  cat(texto,file=paste0("plot_results/html/",red,".html"),sep="\n")
}

ref_pol <- read.csv("plot_results/references_pol_purg.csv")#, sep=";")
ref_sd <- read.csv("plot_results/references_sd.csv")
ref_tot <<- rbind(ref_pol,ref_sd)

ficheros <- Sys.glob("data/M*.csv")
for (j in ficheros)
{
  red <- strsplit(j,"/")[[1]][2]
  red_name <- strsplit(red,".csv")[[1]][1]
  description <- ref_tot[ref_tot$ID == red_name,]
  pos <- which(ref_tot$ID == red_name)
  print(pos)
  prevnet <- ifelse(pos>1, as.character(ref_tot[pos-1,]$ID) ,NA)
  nextnet <- ifelse(pos<nrow(ref_tot),as.character(ref_tot[pos+1,]$ID),NA)
  gen_html_file(red_name,description,nextnet,prevnet)
}