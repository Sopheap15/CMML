# Load data ( select all => crt/cmd + enter) ----
source("code/Load_data.R",local = knitr::knit_global())

# Render report
rmarkdown::render(
  input = "code/CMML_report.Rmd", encoding = 'UTF-8',
  output_file = paste0("../Output/CMML Report ",Sys.Date(),".html"))