# map data-----
# read boundary map
map <- list.files("khm_adm1_un", pattern = "khm_adm1_un.shp", full.names = T) %>% 
	read_sf()
map <- sf::st_make_valid(map)

# data
m_data <- data %>% 
	filter(!is.na(customer), delivery_date >= date[3, 2], delivery_date < date[4, 2]) %>% 
	mutate(customer = recode(customer,
													 "BTB_AFRIMS" = "Battambang",
													 "National Public Health Laboratory - AMR" = "NPHL",
													 "National Public Health Laboratory - EGASP" = "NPHL",
													 "NHealth Laboratory" = "Royal Phnom Penh hospital" )) %>%
	group_by(customer, x, y) %>%
	summarise(Total = sum(d_quantity, na.rm = T)) %>% 
	drop_na() %>% 
	st_as_sf(coords = c("x","y"), crs = 4326)