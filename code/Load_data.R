# Load library----
library(tidyverse)
library(lubridate)
library(rio)
library(janitor)
library(kableExtra)
library(ggrepel)
library(sf)
library(tmap)
library(googlesheets4)

# Analysis date----
date <- list.files("dictionary", pattern = "^[dD]ic.*.xls(x)?", full.names = T) %>% 
	import(sheet = "date") %>% 
	clean_names() %>% 
	mutate(date_dd_mmm_yyyy = as.Date(date_dd_mmm_yyyy))

# Load script----
source("code/production_delivery.R", local = knitr::knit_global())
source("code/save_data_to_excel.R", local = knitr::knit_global())
source("code/raw_material.R", local = knitr::knit_global())
source("code/sheep.R", local = knitr::knit_global())
source("code/iqc.R", local = knitr::knit_global())
source("code/map.R", local = knitr::knit_global())


