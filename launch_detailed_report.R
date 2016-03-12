library(knitr)
library(rmarkdown)
library(stringr)
arg_netname <<- "M_PL_001"
ficheros <- str_replace(str_replace(Sys.glob("data/M*.csv"),"data/",""),".csv","")
for (arg_netname in ficheros)
{
  print("Generating detailed report")
  print(arg_netname)
  render('detailed_report.Rmd')
  file.rename("detailed_report.pdf", paste0("detailed_reports/",arg_netname,"detailed_report.pdf"))
}