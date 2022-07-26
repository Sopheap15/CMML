
# Raw material data -------
r_data <- list.files("data", pattern = "^[Rr]aw.*.csv", full.names = T)
r_date <- lubridate::dmy(str_extract(r_data,"\\d+[.-:/ ]\\d+[.-:/ ]\\d+"))

r_data <- r_data %>% 
	read_csv2(trim_ws = T, na = c("", "NA")) %>% 
	clean_names()

# clean raw material data
r_data <- r_data %>% 
		mutate(name = case_when(
			str_detect(str_to_lower(product_name), "3m attest") == T ~ "3M attest biological indicator",
			str_detect(str_to_lower(product_name), "50ml plug-seal") == T ~ "50ml conic centrifuge tube",
			str_detect(str_to_lower(product_name), "acetic acid glacial") == T ~ "Acetic acid glacial",
			str_detect(str_to_lower(product_name), "agar n1") == T ~ "Agar N1",
			str_detect(str_to_lower(product_name), "airway needle") == T ~ "Aiway needle",
			str_detect(str_to_lower(product_name), "alcohol gel") == T ~ "Acohol gel",
			str_detect(str_to_lower(product_name), "alcohol pad") == T ~ "Acohol pad",
			str_detect(str_to_lower(product_name), "bhi cap") == T ~ "Aluminum cap BHI bottle 20mm",
			str_detect(str_to_lower(product_name), "aluminum foil") == T ~ "Aluminum foil",
			str_detect(str_to_lower(product_name), "amies transport") == T ~ "Amies Transport Medium",
			str_detect(str_to_lower(product_name), "autoclave bag 30x60") == T ~ "Autoclave bag 30x60cm",
			str_detect(str_to_lower(product_name), "autoclave bags 14") == T ~ "Autoclave bags 14L",
			str_detect(str_to_lower(product_name), "azithromycin") == T ~ "Azithromycin 15mg",
			str_detect(str_to_lower(product_name), "betadine") == T ~ "Betadine",
			str_detect(str_to_lower(product_name), "injection glass vial 100ml") == T ~ "BHI glass bottle 100ml",
			str_detect(str_to_lower(product_name), "injection glass vial 50ml") == T ~ "BHI glass bottle 50ml",
			str_detect(str_to_lower(product_name), "blood agar base(?!.*2)") == T ~ "Blood agar base",
			str_detect(str_to_lower(product_name), "blood bag 250ml") == T ~ "Blood bag 250ml",
			str_detect(str_to_lower(product_name), "borosilicate glass") == T ~ "Borosilicate tube 13x100ml",
			str_detect(str_to_lower(product_name), "brain heart infusion") == T ~ "Brain Heart Infusion Broth",
			str_detect(str_to_lower(product_name), "cefixim") == T ~ "Cefixime 5mg",
			str_detect(str_to_lower(product_name), "ceftriaxone") == T ~ "Ceftriaxone 30mg",
			str_detect(str_to_lower(product_name), "ciprofloxacin") == T ~ "Ciprofloxacine 5mg",
			str_detect(str_to_lower(product_name), "tape clear") == T ~ "Clear scotch tape",
			str_detect(str_to_lower(product_name), "cled agar") == T ~ "CLED agar w/bromothymol blue",
			str_detect(str_to_lower(product_name), "(cotrimoxazole)|(trimethoprim)") == T ~ "Cotrimoxazole 25mg",
			str_detect(str_to_lower(product_name), "100-well cryogenic") == T ~ "Cryobox 100 wells",
			str_detect(str_to_lower(product_name), "crystal violet (?!0.1)") == T ~ "Crystal Violet",
			str_detect(str_to_lower(product_name), "crystal violet") == T ~ "Crystal Violet 0.1% (in house)",
			str_detect(str_to_lower(product_name), "gc agar") == T ~ "GC agar base",
			str_detect(str_to_lower(product_name), "gentamicin.*10") == T ~ "Gentamicin 10mg",
			str_detect(str_to_lower(product_name), "gentamicin.*80") == T ~ "Gentamicin 80mg",
			str_detect(str_to_lower(product_name), "(.*)?glove.+m") == T ~ "Glove (M) powder-free",
			str_detect(str_to_lower(product_name), "(.*)?glove.+s") == T ~ "Glove (S) powder-free",
			str_detect(str_to_lower(product_name), "(.*)?glove.*free(?!.)") == T ~ "Glove (?) powder-free",
			str_detect(str_to_lower(product_name), "(.*)?gloves latex") == T ~ "Glove (?) powdered",
			str_detect(str_to_lower(product_name), "glycerol") == T ~ "Glycerol",
			str_detect(str_to_lower(product_name), "haem.*globin") == T ~ "Haemoglobin powder",
			str_detect(str_to_lower(product_name), "hand towel") == T ~ "Hand towel",
			str_detect(str_to_lower(product_name), "hektoen") == T ~ "Hektoen enteric agar",
			str_detect(str_to_lower(product_name), "hydrochloric acid") == T ~ "Hydrochloric Acid (HCl)",
			str_detect(str_to_lower(product_name), "thiourea") == T ~ "Hydrochloric Acid (HCl)",
			str_detect(str_to_lower(product_name), "kligler.*agar") == T ~ "Kligler Iron Agar",
			str_detect(str_to_lower(product_name), "lysine.*agar") == T ~ "Lysine Iron Agar",
			str_detect(str_to_lower(product_name), "kovacs indole") == T ~ "Kovacs indole Reagent",
			str_detect(str_to_lower(product_name), "label.*20mm x 15mm") == T ~ "Label sticker 20x15mm",
			str_detect(str_to_lower(product_name), "label.*25mm x 15mm") == T ~ "Label sticker 25x15mm",
			str_detect(str_to_lower(product_name), "label.*84mm x 41mm") == T ~ "Sleeve label 84x41mm",
			str_detect(str_to_lower(product_name), "loop 10") == T ~ "Loop 10ul",
			str_detect(str_to_lower(product_name), "loop 1") == T ~ "Loop 1ul",
			str_detect(str_to_lower(product_name), "macconkey") == T ~ "MacConkey Agar",
			str_detect(str_to_lower(product_name), "mannitol salt") == T ~ "Mannitol Salt Agar",
			str_detect(str_to_lower(product_name), "mask protective") == T ~ "Surgical Mask",
			str_detect(str_to_lower(product_name), "mask.*95") == T ~ "VN95 Mask",
			str_detect(str_to_lower(product_name), "mueller.*broth") == T ~ "Mueller Hinton Broth",
			str_detect(str_to_lower(product_name), "mueller.*agar") == T ~ "Mueller Hinton II Agar",
			str_detect(str_to_lower(product_name), "needle 21g") == T ~ "Needle 21G",
			str_detect(str_to_lower(product_name), "neutral red\\s+?(?!\\d)") == T ~ "Neutral Red",
			str_detect(str_to_lower(product_name), "neutral red\\s+?\\d") == T ~ "Neutral Red 1%",
			str_detect(str_to_lower(product_name), "petri dish") == T ~ "Petri dish",
			str_detect(str_to_lower(product_name), "pipette.*3ml sterile") == T ~ "Pipette plastic 3ml sterile",
			str_detect(str_to_lower(product_name), "pipette.*1000") == T ~ "Pipette Tip sterile 1000ul",
			str_detect(str_to_lower(product_name), "pipette.*200") == T ~ "Pipette Tip sterile 200ul",
			str_detect(str_to_lower(product_name), "kcl 3") == T ~ "Potassium chloride (KCl)",
			str_detect(str_to_lower(product_name), "anti-bubble") == T ~ "Pourite anti-bubble additive",
			str_detect(str_to_lower(product_name), "ribbon.*110mm x 300m.*") == T ~ "Ribbon 110x 300m",
			str_detect(str_to_lower(product_name), "ribbon.*27mm x 300m.*") == T ~ "Ribbon 27x 300m",
			str_detect(str_to_lower(product_name), "rubber stopper") == T ~ "Rubber stopper BHI 20mm",
			str_detect(str_to_lower(product_name), "sabouraud.*glucose") == T ~ "Sabouraud 4% Glucose Agar",
			str_detect(str_to_lower(product_name), "sabouraud.*chloram.*") == T ~ "Sabouraud Chloramphenicol Agar",
			str_detect(str_to_lower(product_name), "plastic tube 12") == T ~ "Screw cap plastic tube 12ml",
			str_detect(str_to_lower(product_name), "sim medium") == T ~ "SIM medium",
			str_detect(str_to_lower(product_name), "simmon.*citrate") == T ~ "Simmons Citrate Agar",
			str_detect(str_to_lower(product_name), "hand.*wash") == T ~ "Liquid hand soap",
			str_detect(str_to_lower(product_name), "sodium chloride") == T ~ "Sodium chloride (NaCl)",
			str_detect(str_to_lower(product_name), "sodium hydroxide") == T ~ "Sodium hydroxide (NaOH)",
			str_detect(str_to_lower(product_name), "sodium polyane") == T ~ "Sodium Polyanethole Sulfate (SPS)",
			str_detect(str_to_lower(product_name), "soyabean.*agar") == T ~ "Soyabean Casein Digest Agar (TSA)",
			str_detect(str_to_lower(product_name), "soyabean.*medium") == T ~ "Soyabean Casein Digest Medium (TSB)",
			str_detect(str_to_lower(product_name), "2.*cryovial") == T ~ "Sterile screw tube 2ml",
			str_detect(str_to_lower(product_name), "gauze") == T ~ "Sterilized gauze",
			str_detect(str_to_lower(product_name), "syringe 5") == T ~ "Syringe 5cc",
			str_detect(str_to_lower(product_name), "syringe 10") == T ~ "Syringe 10cc",
			str_detect(str_to_lower(product_name), "syringe 20") == T ~ "Syringe 20cc",
			str_detect(str_to_lower(product_name), "thiosulfate citrate") == T ~ "TCBS",
			str_detect(str_to_lower(product_name), "tetracycl") == T ~ "Tetracycline 30mg",
			str_detect(str_to_lower(product_name), "triple packaging") == T ~ "Triple packaging",
			str_detect(str_to_lower(product_name), "tryptic.* blood agar.*") == T ~ "Tryptic Soy Blood Agar Base No 2",
			str_detect(str_to_lower(product_name), "tryptic.*broth") == T ~ "Tryptic Soy Broth",
			str_detect(str_to_lower(product_name), "tryptic.*agar") == T ~ "Trypticase Soy Agar",
			str_detect(str_to_lower(product_name), "urea agar") == T ~ "Urea agar base",
			str_detect(str_to_lower(product_name), "urea.*40%") == T ~ "Urea supplement 40%",
			str_detect(str_to_lower(product_name), "vcnt.*supplement") == T ~ "VCNT supplement",
			str_detect(str_to_lower(product_name), "(vitamino.*supplement)|(vitox)") == T ~ "Vitamino supplement",
			str_detect(str_to_lower(product_name), "kimcap.*13") == T ~ "KimCap 13mm",
			str_detect(str_to_lower(product_name), "colomycin") == T ~ "colistin",
			TRUE ~ product_name
			
		)) 


# Read raw material from dictionary
raw_material <- list.files("dictionary", 
													 pattern = "^[dD]ic.*.xls(x)?", 
													 full.names = T) %>% 
	import(sheet = "raw_material")

# combine raw data and dictionary
r_data <- merge(r_data, raw_material, by = "name", all = T)

#remove object
rm(raw_material)
