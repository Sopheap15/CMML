# Read data from media production----
p_data <- list.files("data", pattern = ".*prod.*.csv", full.names = T) %>% 
	read_csv2(trim_ws = T, na = c("", "NA")) %>% 
	clean_names() %>% 
	rename("p_quantity" = quantity)


# Read data from media delivery----
d_data <- list.files("data", pattern = ".*deliv.*.csv", full.names = T) %>% 
	read_csv2(trim_ws = T, na = c("", "NA")) %>% 
	clean_names() %>% 
	rename("d_quantity" = quantity) %>% 
	mutate(delivery_date = as.Date(delivery_date, format = "%Y-%m-%d"),
				 customer_name = str_replace(customer_name, pattern = "[Ff]onda.*M.*", replacement = "Fondation Merieux"))

# Read customer data----
customer <- list.files("dictionary", pattern = "^[dD]ic.*.xls(x)?", full.names = T) %>%
	import(sheet = "customer") %>% 
	clean_names()

# Merge delivery and customer
d_data <- merge(d_data, customer, by.x = "customer_name", by.y = "cmmlms") %>% view()

# Merge production and delivery----
data <-	full_join(p_data, d_data) %>% 
	mutate(p_month = factor(format(preparation_date, "%b"), levels = month.abb, ordered = T),
				 d_month = factor(format(delivery_date,"%b"), levels = month.abb, ordered = T))

## Clean Production data----
data <- data %>% 
	mutate(
		# Mutate media name
		powder_name = case_when(
			str_detect(media_name,"(?i)Amies Transport")==T ~ "ATM",
			str_detect(media_name,"(?i)Blood Culture")==T ~ "BHI",
			str_detect(media_name,"(?i)Blood Agar") ==T ~ "BAB2",
			str_detect(media_name,"(?i)Chocolate") ==T ~ "BAB infusion",
			str_detect(media_name,"(?i)MacConkey") ==T ~ "MAC",
			str_detect(media_name,"(?i)Mannitol Salt") ==T ~ "MSA",
			str_detect(media_name,"(?i)Mueller[ -]Hinton Agar") ==T ~ "MHII agar",
			str_detect(media_name,"(?i)Mueller[ -]Hinton Broth") ==T ~ "MH broth",
			str_detect(media_name,"(?i)Ashdown's Agar") ==T ~ "TSA",
			str_detect(media_name,"(?i)Cystine Lactose") ==T ~ "CLED",
			str_detect(media_name,"(?i)Thiosulfate Citrate") ==T ~ "TCBS",
			str_detect(media_name,"(?i)Hektoen") ==T ~ "HEK",
			str_detect(media_name,"(?i)Modified Thayer-Martin") ==T ~ "MTM",
			str_detect(media_name,"(?i)GC") ==T ~ "GC",
			str_detect(media_name,"(?i)Kligler") ==T ~ "KIA",
			str_detect(media_name,"(?i)Lysine") ==T ~ "LIA",
			str_detect(media_name,"(?i)Ashdown's Broth") ==T ~ "TSB",
			str_detect(media_name,"(?i)Simmons Citrate") ==T ~ "CIT",
			str_detect(media_name,"(?i)Sulfide Indole") ==T ~ "SIM",
			str_detect(media_name,"(?i)Trypticase Soy Agar") ==T ~ "TSA",
			str_detect(media_name,"(?i)Trypticase Soy Broth") ==T ~ "TSB",
			str_detect(media_name,"(?i)Urea Agar") ==T ~ "Urea",
			TRUE ~ media_name
		),
		# Mutate media type
		media_type = case_when(
			str_detect(media_name,"(?i)Blood.*adult")==T ~ "Adult Bottle",
			str_detect(media_name,"(?i)Blood.*child")==T ~ "Child Bottle",
			str_detect(media_name,"(?i)Blood Agar|(?i)Chocolate|(?i)MacConkey|(?i)Mannitol Salt|(?i)Mueller.*Agar|(?i)Ashdown's Agar|(?i)Cystine Lactose|(?i)Thiosulfate Citrate|(?i)Hektoen|(?i)Modified Thayer-Martin|(?i)GC")==T ~ "Plate",
			str_detect(media_name,"(?i)Kligler|(?i)Lysine|(?i)Ashdown's Broth|(?i)Simmons Citrate(?i)|(?i)Sulfide Indole(?i)|(?i)Trypticase Soy Agar|(?i)Trypticase Soy Broth(?i)|(?i)Urea Agar|(?i)Mueller.*Broth|(?i)Amies Transport")==T ~ "Tube",
			TRUE ~ "unknown" # new media type is known
		),
		
		# Powder calculation.......
		# p_quanity* powder in 1L/1000 * volume in a container. see comment in MH

		powder_kg = case_when(
			media_name == "Amies Transport Media" ~ p_quantity * 0.1/1000,
			media_name == "Ashdown's Agar" ~ p_quantity*0.88/1000,
			media_name == "Ashdown's Broth" ~ p_quantity*0.88/1000,
			media_name == "Blood Agar" ~ p_quantity*0.88/1000,
			media_name == "Blood Culture Bottle (adult)" ~ (p_quantity*50*37)/1000000,
			media_name == "Blood Culture Bottle (child)" ~ (p_quantity*25*37)/1000000,
			media_name == "Chocolate Agar" ~ p_quantity*0.88/1000,
			media_name == "Cystine Lactose Electrolyte Deficient Agar" ~ p_quantity*0.8/1000,
			#media_name == "CLED" ~ p_quantity*0.8/1000,
			media_name == "Hektoen Enteric Agar" ~ p_quantity*1.7/1000,
			media_name == "Kligler Iron Agar" ~ p_quantity*0.21/1000,
			media_name == "Lysine Iron Agar" ~ p_quantity*0.14/1000,
			media_name == "MacConkey Agar" ~ p_quantity*1.15/1000,
			media_name == "Mannitol Salt Agar" ~ p_quantity*2.46/1000,
			media_name == "Mueller Hinton II Agar" ~ p_quantity*0.95/1000, # 38*25/1000000
			media_name == "Mueller Hinton Broth" ~ p_quantity*0.063/1000,
			media_name == "Sheep Blood Mueller-Hinton Agar" ~ p_quantity*0.95/1000,
			media_name == "Simmons Citrate Agar" ~ p_quantity*0.07/1000,
			media_name == "Sulfide Indole Motility Medium" ~ p_quantity*0.09/1000,
			media_name == "Thiosulfate Citrate Bile Salts Sucrose Agar" ~ p_quantity*1.96/1000,
			#media_name == "TCBS" ~ p_quantity*1.96/1000,
			media_name == "Trypticase Soy Agar" ~ p_quantity*0.04/1000,
			media_name == "Urea Agar" ~ p_quantity*0.07/1000, 
			media_name == "Trypticase Soy Broth" ~ p_quantity * 0.04/1000,
			media_name == "Trypticase Soy Broth + 20% glycerol" ~ p_quantity * 0.04/1000,
			media_name == "Modified Thayer-Martin Agar" ~ p_quantity * 0.88/1000,
			media_name == "GC agar +1% growth supplement" ~ p_quantity * 0.8/1000,
			
			
		)) # close mutate

## clean Delivery data---------

data <- data %>% 
	mutate(
		# recode media name..................
		media_name = recode(media_name, 
												"Thiosulfate Citrate Bile Salts Sucrose Agar" = "TCBS",
												"Cystine Lactose Electrolyte Deficient Agar"  = "CLED"),
		# calculate media cost
		cost = case_when(
			media_name == "Amies Transport Media"  ~ d_quantity * 0.85,
			media_name == "Ashdown's Agar" ~ d_quantity * 0.99,
			media_name == "Ashdown's Broth" ~ d_quantity * 0.76,
			media_name == "Blood Agar" ~ d_quantity * 1.1,
			media_name == "Blood Culture Bottle (adult)" ~ d_quantity * 1.72,
			media_name == "Blood Culture Bottle (child)" ~ d_quantity * 1.62,
			media_name == "Chocolate Agar" ~ d_quantity * 1.2,
			media_name == "CLED" ~ d_quantity * 0.69,
			media_name == "GC agar +1% growth supplement"  ~ d_quantity * 1.91,
			media_name == "Hektoen Enteric Agar" ~ d_quantity * 0.8,
			media_name == "Kligler Iron Agar" ~ d_quantity * 0.8,
			media_name == "Lysine Iron Agar" ~ d_quantity * 0.84,
			media_name == "MacConkey Agar" ~ d_quantity * 0.65,
			media_name == "Mannitol Salt Agar" ~ d_quantity * 0.88,
			media_name == "Mueller Hinton II Agar" ~ d_quantity * 0.9,
			media_name == "Mueller Hinton Broth" ~ d_quantity * 0.82,
			media_name == "Modified Thayer-Martin Agar"  ~ d_quantity * 2.39,
			media_name == "Sheep Blood Mueller-Hinton Agar" ~ d_quantity * 1.41,
			media_name == "Simmons Citrate Agar" ~ d_quantity * 0.76,
			media_name == "Sulfide Indole Motility Medium" ~ d_quantity * 0.83,
			media_name == "TCBS" ~ d_quantity * 1.23,
			media_name == "Trypticase Soy Agar" ~ d_quantity * 0.66,
			media_name == "Trypticase Soy Broth"  ~ d_quantity * 0.76,
			media_name == "Trypticase Soy Broth + 20% glycerol"  ~ d_quantity * 0.66,
			media_name == "Urea Agar" ~ d_quantity * 1.04
			
		))
