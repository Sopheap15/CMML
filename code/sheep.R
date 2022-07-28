# load sheep data----------------
gs4_deauth()
#gs4_auth(email = "oeng.sopheap@dmdp.org")
url <- "https://docs.google.com/spreadsheets/d/1ixEijAjGdYoIG56HROUVuN9z_DXIak1A6achRbUAzFM/edit#gid=1304838431"
# read sheep inventory
sheep_inventory <- read_sheet(url, sheet = "Sheep_inventory", skip = 2) %>%
	clean_names() %>% 
	mutate(comment1 = tolower(str_extract(comment, "dead|Dead"))) %>% 
	filter(is.na(comment))

# read sheep care
sheep_care <- read_sheet(url, sheet = "Sheep_care", skip = 2) %>% 
	clean_names() %>% 
	filter(care_date >= date[1, 2], 
				 care_date < date[2, 2]) 

# sheep blood collection
sheep_blood <- read_sheet(url, sheet = "Sheep_blood_collection", skip = 2) %>% 
	select(Collection_Date:Comment) %>% 
	clean_names() %>% 
	filter(collection_date >= date[1, 2], 
				 collection_date < date[2, 2]) 