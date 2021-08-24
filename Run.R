# Load library
library(here)
library(tidyverse)
library(lubridate)
library(rio)
library(janitor)
library(kableExtra)
library(ggrepel)
library(sf)
library(tmap)
library(googlesheets4)
library(rstatix)

# Load data

source("code/Load_data.R",local = knitr::knit_global())

# Render report
rmarkdown::render(
  input = "code/CMML_report.Rmd",
  output_file = paste("../Output/CMML Report",Sys.Date(),".html")) 

