# Load library----
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

# Load production, delivery data and bind together--------------------

# read data from media production
p_data <- read_csv(here("data","Media_production.csv"),trim_ws = T,na = c("","NA")) %>% clean_names() %>% rename("p_quantity" = quantity)


# read data from media delivery
d_data <- read_csv(here("data","Media_delivery.csv"),trim_ws = T,na = c("","NA")) %>% clean_names() %>% rename("d_quantity" = quantity)

# Full joint data together
data <- full_join(p_data,d_data,by.x = "media_name", by.y = "media_name") %>% 
  mutate(p_month = factor(format(preparation_date,"%b"),levels = month.abb, ordered = T),
         d_month = factor(format(delivery_date,"%b"),levels = month.abb, ordered = T))

  # clean Production data-------------------
data <- data %>%
  # dehydrate powder name.......
  mutate(powder_name = recode(media_name,
                              # media bottle
                              "Blood Culture Bottle (adult)" = "BHI",
                              "Blood Culture Bottle (child)" = "BHI",
                              # media plate
                              "Blood Agar" = "BAB2",
                              "Chocolate Agar" = "BAB infusion",
                              "MacConkey Agar" = "MAC",
                              "Mannitol Salt Agar" = "MSA",
                              "Mueller Hinton II Agar" = "MHII",
                              "Sheep Blood Mueller-Hinton Agar" = "MHII",
                              "Ashdown's Agar" = "TSA",
                              "Cystine Lactose Electrolyte Deficient Agar" = "CLED",
                              "Thiosulfate Citrate Bile Salts Sucrose Agar" = "TCBS",
                              "Hektoen Enteric Agar" = "HEK",
                              "Modified Thayer-Martin Agar" = "MTM",
                              "GC agar +1% growth supplement" = "GC",
                              # media tube
                              "Kligler Iron Agar" = "KIA",
                              "Lysine Iron Agar" = "LIA",
                              "Ashdown's Broth" = "TSB",
                              "Simmons Citrate Agar" = "CIT",
                              "Sulfide Indole Motility Medium" = "SIM",
                              "Trypticase Soy Agar" = "TSA",
                              "Trypticase Soy Broth" = "TSB",
                              "Trypticase Soy Broth + 20% glycerol" = "TSB",
                              "Urea Agar" = "Urea"
  ),
  # define media type.......
  media_type = recode(media_name,
                      # media bottle
                      "Blood Culture Bottle (adult)" = "Adult Bottle",
                      "Blood Culture Bottle (child)" = "Child Bottle",
                      # media plate
                      "Blood Agar" = "Plate",
                      "Chocolate Agar" = "Plate",
                      "MacConkey Agar" = "Plate",
                      "Mannitol Salt Agar" = "Plate",
                      "Mueller Hinton II Agar" = "Plate",
                      "Sheep Blood Mueller-Hinton Agar" = "Plate",
                      "Ashdown's Agar" = "Plate",
                      "Cystine Lactose Electrolyte Deficient Agar" = "Plate",
                      "Modified Thayer-Martin Agar" = "Plate",
                      "Hektoen Enteric Agar" = "Plate",
                      "Thiosulfate Citrate Bile Salts Sucrose Agar" = "Plate",
                      "GC agar +1% growth supplement" = "Plate",
                      # media tube
                      "Kligler Iron Agar" = "Tube",
                      "Lysine Iron Agar" = "Tube",
                      "Ashdown's Broth" = "Tube",
                      "Simmons Citrate Agar" = "Tube",
                      "Sulfide Indole Motility Medium" = "Tube",
                      "Trypticase Soy Agar" = "Tube",
                      "Trypticase Soy Broth" = "Tube",
                      "Trypticase Soy Broth + 20% glycerol" = "Tube",
                      "Urea Agar" = "Tube"
  ),
  # Powder calculation.......
  powder_kg = case_when(
    media_name == "Blood Agar" ~ p_quantity*0.88/1000,
    media_name == "Ashdown's Agar" ~ p_quantity*0.88/1000,
    media_name == "Ashdown's Broth" ~ p_quantity*0.88/1000,
    media_name == "Blood Culture Bottle (adult)" ~ (p_quantity*50*37)/1000000,
    media_name == "Blood Culture Bottle (child)" ~ (p_quantity*25*37)/1000000,
    media_name == "Chocolate Agar" ~ p_quantity*0.88/1000,
    media_name == "Cystine Lactose Electrolyte Deficient Agar" ~ p_quantity*0.8/1000,
    media_name == "CLED" ~ p_quantity*0.8/1000,
    media_name == "Hektoen Enteric Agar" ~ p_quantity*1.7/1000,
    media_name == "Kligler Iron Agar" ~ p_quantity*0.21/1000,
    media_name == "Lysine Iron Agar" ~ p_quantity*0.14/1000,
    media_name == "MacConkey Agar" ~ p_quantity*1.15/1000,
    media_name == "Mannitol Salt Agar" ~ p_quantity*2.46/1000,
    media_name == "Mueller Hinton II Agar" ~ p_quantity*0.95/1000,
    media_name == "Sheep Blood Mueller-Hinton Agar" ~ p_quantity*0.95/1000,
    media_name == "Simmons Citrate Agar" ~ p_quantity*0.07/1000,
    media_name == "Sulfide Indole Motility Medium" ~ p_quantity*0.09/1000,
    media_name == "Thiosulfate Citrate Bile Salts Sucrose Agar" ~ p_quantity*1.96/1000,
    media_name == "TCBS" ~ p_quantity*1.96/1000,
    media_name == "Trypticase Soy Agar" ~ p_quantity*0.04/1000,
    media_name == "Urea Agar" ~ p_quantity*0.07/1000, 
    media_name == "Trypticase Soy Broth" ~ p_quantity * 0.04/1000,
    media_name == "Trypticase Soy Broth + 20% glycerol" ~ p_quantity * 0.04/1000,
    media_name == "Modified Thayer-Martin Agar" ~ p_quantity * 0.88/1000,
    media_name == "GC agar +1% growth supplement" ~ p_quantity * 0.8/1000
    
  )
  )


  # clean Delivery data--------------------
data <- data %>% 
  # recode customer name......
  mutate(
    customer_name = recode(customer_name, 
                           "AFRIM Project BTB"  = "BTB_AFRIMS",
                           "National Public Health Laboratory Microbiology Unit"  = "NPHL",
                           "Sonua Kill Memorial Hospital" = "Sonja kill",
                           "Central Media Making Laboratory" = "CMML",
                           "Khmer Soviet Friendship Hospital" = "KSFH",
                           "Sihanouk Hospital Center of Hope" = "SHCH",
                           "Diagnostic and Detection Laboratory" = "DDL"
    ),
    # type of customer......
    customer_type = recode(customer_name,
                           #Government Lab supported by DMDP
                           "Siem Reap" = "Govt. Lab supported by DMDP",
                           "Takeo"     = "Govt. Lab supported by DMDP",
                           "Battambang"  = "Govt. Lab supported by DMDP",
                           "Kampong Cham"  = "Govt. Lab supported by DMDP",
                           "NPHL" = "Govt. Lab supported by DMDP",
                           
                           #Government lab purchase
                           "KSFH" = "Govt. Lab (purchased)",
                           "National Pediatric Hospital" = "Govt. Lab (purchased)",
                           "Kossamak" = "Govt. Lab (purchased)",
                           "Svay Rieng" = "Govt. Lab (purchased)",
                           
                           #Government Lab supported by project
                           "SHCH" = "Govt. Lab supported by project",
                           "US NAMRU-2" = "Govt. Lab supported by project",
                           "BTB_AFRIMS" = "Govt. Lab supported by project",
                           "National Public Health Laboratory - AMR" = "Govt. Lab supported by project",
                           
                           # Private Lab (purchased)
                           "Sunrise Hospital" = "Private Lab (purchased)",
                           "Biobykin" = "Private Lab (purchased)",
                           "Amatak Laboratory" = "Private Lab (purchased)",
                           "International Standard lab" = "Private Lab (purchased)",
                           "BOM" = "Private Lab (purchased)",
                           "Institute Pasteur Cambodge" = "Private Lab (purchased)",
                           "Pro Labpratory" = "Private Lab (purchased)",
                           "DDL" = "Private Lab (purchased)",
                           "Saravorn Laboratory" = "Private Lab (purchased)",
                           "Urology clinic" = "Private Lab (purchased)",
                           "Sonja kill" = "Private Lab (purchased)",
                           "Central Laboratory" = "Private Lab (purchased)",
                           "Dynamic Pharma" = "Private Lab (purchased)",
                           "NHealth Laboratory" = "Private Lab (purchased)",
                           "American University of Phnom penh" = "Private Lab (purchased)",
                           "Olympia Medical Hub" = "Private Lab (purchased)",
                           "Gold Medical Diagnostic Laboratory" = "Private Lab (purchased)",
                           
                           # Training or promotion
                           "DMDP training" = "Training/promotion",
                           "CMML" = "Training/promotion",
                           "IQLS" = "Training/promotion"
    ),
    
    # recode media name..................
    media_name = recode(media_name, 
                        "Thiosulfate Citrate Bile Salts Sucrose Agar" = "TCBS",
                        "Cystine Lactose Electrolyte Deficient Agar"  = "CLED"),
    
    
    # calculate media cost
    cost = case_when(
      media_name == "Blood Agar" ~ d_quantity * 1.1,
      media_name == "Ashdown's Agar" ~ d_quantity * 0.99,
      media_name == "Ashdown's Broth" ~ d_quantity * 0.76,
      media_name == "Blood Culture Bottle (adult)" ~ d_quantity * 1.72,
      media_name == "Blood Culture Bottle (child)" ~ d_quantity * 1.62,
      media_name == "Chocolate Agar" ~ d_quantity * 1.2,
      media_name == "Cystine Lactose Electrolyte Deficient Agar" ~ d_quantity * 0.69,
      media_name == "CLED" ~ d_quantity * 0.69,
      media_name == "Hektoen Enteric Agar" ~ d_quantity * 0.8,
      media_name == "Kligler Iron Agar" ~ d_quantity * 0.8,
      media_name == "Lysine Iron Agar" ~ d_quantity * 0.84,
      media_name == "MacConkey Agar" ~ d_quantity * 0.65,
      media_name == "Mannitol Salt Agar" ~ d_quantity * 0.88,
      media_name == "Mueller Hinton II Agar" ~ d_quantity * 0.9,
      media_name == "Sheep Blood Mueller-Hinton Agar" ~ d_quantity * 1.41,
      media_name == "Simmons Citrate Agar" ~ d_quantity * 0.76,
      media_name == "Sulfide Indole Motility Medium" ~ d_quantity * 0.83,
      media_name == "Thiosulfate Citrate Bile Salts Sucrose Agar" ~ d_quantity * 1.23,
      media_name == "TCBS" ~ d_quantity * 1.23,
      media_name == "Trypticase Soy Agar" ~ d_quantity * 0.66,
      media_name == "Urea Agar" ~ d_quantity * 1.04 
      ## no cost for TSB, TSBG, GC, MTM
    )
  )


# load Raw material data -------
# read raw materials file
r_data <- read_csv(here("data","Raw_material.csv"), trim_ws = T, na = c("","NA")) %>% clean_names()

# read threshold
#threshold <- read_csv("stock_threshold.csv",trim_ws = T, na = c("","NA"))
dic <- readxl::read_excel(here("dictionary","dictionary.xlsx"), trim_ws = T, na = c("","NA"))

r_data <- merge(r_data, dic, by = "product_name", all = T)




# IQC----
# raw data from system and filter to date > 2021-01-01
iqc <- import(here("data","IQC.csv")) %>% 
  mutate(qc_date = as.Date(qc_date, "%d/%m/%y")) %>% 
  filter(qc_date >= "2021-01-01", qc_result != "", !batch_no %in% c("	
KIA260521-01 ", "KIA260521-01", "KIA030621-01", "KIA090621-01")) %>% 
  mutate(exp = str_extract(comments,"Experiment|experiment|exper|Exper"))


# load sheep data----------------

gs4_auth(email = "oeng.sopheap@dmdp.org")
url <- "https://docs.google.com/spreadsheets/d/1ixEijAjGdYoIG56HROUVuN9z_DXIak1A6achRbUAzFM/edit#gid=1304838431"
# read sheep inventory
sheep_inventory <- read_sheet(url, sheet = "Sheep_inventory", skip = 2) %>%
  clean_names() %>% 
  mutate(comment1 = tolower(str_extract(comment, "dead|Dead"))) %>% 
  filter(is.na(comment))

# read sheep care
sheep_care <- read_sheet(url, sheet = "Sheep_care", skip = 2) %>% 
  clean_names()

# sheep blooc collection
sheep_blood <- read_sheet(url, sheet = "Sheep_blood_collection", skip = 2) %>% 
  select(Collection_Date:Comment) %>% 
  clean_names()


# map data-----
# read boundary map
map <- read_sf(here("khm_adm1_un","khm_adm1_un.shp"))
map <- sf::st_make_valid(map)

# data
m_data <- data %>% 
  filter(!is.na(customer_name)) %>% 
  mutate(customer_name = recode(customer_name,
                                "BTB_AFRIMS" = "Battambang",
                                "National Public Health Laboratory - AMR" = "NPHL",
                                "NHealth Laboratory" = "Royal Phnom Penh hospital" )) %>%
  group_by(customer_name) %>%
  summarise(Total = sum(d_quantity,na.rm = T))

# import coordination of customer
adr <- import(here("dictionary","customer_name.xlsx"))

# join data and coordination
m_data <- left_join(m_data, adr) %>% 
  st_as_sf(coords = c("x","y"), crs = 4326)