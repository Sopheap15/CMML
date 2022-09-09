# version 2 reading files from data with ";"
# Load data ( select all => crt/cmd + enter) ----
source("code/Load_data.R", 
			 local = knitr::knit_global())

# Render report
rmarkdown::render(input = "code/CMML_report.Rmd", 
									encoding = "UTF-8",
									output_format = "html_document",
									output_dir = "output",
									output_file = paste0("CMML Report ", Sys.Date()),
									quiet = T)