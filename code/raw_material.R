
# Raw material data -------
r_data <- list.files("data", pattern = "^[Rr]aw.*.csv", full.names = T)
r_date <- lubridate::dmy(str_extract(r_data,"\\d+[.-:/ ]\\d+[.-:/ ]\\d+"))

r_data <- r_data %>% 
	read_csv2(trim_ws = T, na = c("", "NA")) %>% 
	clean_names()

# clean raw material data
r_data <- r_data %>% 
		mutate(name = case_when(
			str_detect(product_name, "(?i)3m attest") == T ~ "3M attest biological indicator",
			str_detect(product_name, "(?i)50ml plug-seal") == T ~ "50ml conic centrifuge tube",
			str_detect(product_name, "(?i)acetic acid glacial") == T ~ "Acetic acid glacial",
			str_detect(product_name, "(?i)agar n1") == T ~ "Agar N1",
			str_detect(product_name, "(?i)airway needle") == T ~ "Aiway needle",
			str_detect(product_name, "(?i)alcohol gel") == T ~ "Acohol gel",
			str_detect(product_name, "(?i)alcohol pad") == T ~ "Acohol pad",
			str_detect(product_name, "(?i)bhi cap") == T ~ "Aluminum cap BHI bottle 20mm",
			str_detect(product_name, "(?i)aluminum foil") == T ~ "Aluminum foil",
			str_detect(product_name, "(?i)amies transport") == T ~ "Amies Transport Medium",
			str_detect(product_name, "(?i)autoclave bag 30x60") == T ~ "Autoclave bag 30x60cm",
			str_detect(product_name, "(?i)autoclave bags 14") == T ~ "Autoclave bags 14L",
			str_detect(product_name, "(?i)azithromycin") == T ~ "Azithromycin 15mg",
			str_detect(product_name, "(?i)betadine") == T ~ "Betadine",
			str_detect(product_name, "(?i)injection glass vial 100ml") == T ~ "BHI glass bottle 100ml",
			str_detect(product_name, "(?i)injection glass vial 50ml") == T ~ "BHI glass bottle 50ml",
			str_detect(product_name, "(?i)blood agar base(?!.*2)") == T ~ "Blood agar base",
			str_detect(product_name, "(?i)blood bag 250ml") == T ~ "Blood bag 250ml",
			str_detect(product_name, "(?i)borosilicate glass") == T ~ "Borosilicate tube 13x100ml",
			str_detect(product_name, "(?i)brain heart infusion") == T ~ "Brain Heart Infusion Broth",
			str_detect(product_name, "(?i)cefixim") == T ~ "Cefixime 5mg",
			str_detect(product_name, "(?i)ceftriaxone") == T ~ "Ceftriaxone 30mg",
			str_detect(product_name, "(?i)ciprofloxacin") == T ~ "Ciprofloxacine 5mg",
			str_detect(product_name, "(?i)tape clear") == T ~ "Clear scotch tape",
			str_detect(product_name, "(?i)cled agar") == T ~ "CLED agar w/bromothymol blue",
			str_detect(product_name, "(?i)(cotrimoxazole)|(trimethoprim)") == T ~ "Cotrimoxazole 25mg",
			str_detect(product_name, "(?i)100-well cryogenic") == T ~ "Cryobox 100 wells",
			str_detect(product_name, "(?i)crystal violet (?!0.1)") == T ~ "Crystal Violet",
			str_detect(product_name, "(?i)crystal violet") == T ~ "Crystal Violet 0.1% (in house)",
			str_detect(product_name, "(?i)gc agar") == T ~ "GC agar base",
			str_detect(product_name, "(?i)gentamicin.*10") == T ~ "Gentamicin 10mg",
			str_detect(product_name, "(?i)gentamicin.*80") == T ~ "Gentamicin 80mg",
			str_detect(product_name, "(?i)glove.+m") == T ~ "Glove (M) powder-free",
			str_detect(product_name, "(?i)glove.+s") == T ~ "Glove (S) powder-free",
			str_detect(product_name, "(?i)glove.*free(?!.)") == T ~ "Glove (?) powder-free",
			str_detect(product_name, "(?i)gloves latex") == T ~ "Glove (?) powdered",
			str_detect(product_name, "(?i)glycerol") == T ~ "Glycerol",
			str_detect(product_name, "(?i)haem.*globin") == T ~ "Haemoglobin powder",
			str_detect(product_name, "(?i)hand towel") == T ~ "Hand towel",
			str_detect(product_name, "(?i)hektoen") == T ~ "Hektoen enteric agar",
			str_detect(product_name, "(?i)hydrochloric acid") == T ~ "Hydrochloric Acid (HCl)",
			str_detect(product_name, "(?i)thiourea") == T ~ "Hydrochloric Acid (HCl)",
			str_detect(product_name, "(?i)kligler.*agar") == T ~ "Kligler Iron Agar",
			str_detect(product_name, "(?i)lysine.*agar") == T ~ "Lysine Iron Agar",
			str_detect(product_name, "(?i)kovacs indole") == T ~ "Kovacs indole Reagent",
			str_detect(product_name, "(?i)label.*20mm x 15mm") == T ~ "Label sticker 20x15mm",
			str_detect(product_name, "(?i)label.*25mm x 15mm") == T ~ "Label sticker 25x15mm",
			str_detect(product_name, "(?i)label.*84mm x 41mm") == T ~ "Sleeve label 84x41mm",
			str_detect(product_name, "(?i)loop 10") == T ~ "Loop 10ul",
			str_detect(product_name, "(?i)loop 1") == T ~ "Loop 1ul",
			str_detect(product_name, "(?i)macconkey") == T ~ "MacConkey Agar",
			str_detect(product_name, "(?i)mannitol salt") == T ~ "Mannitol Salt Agar",
			str_detect(product_name, "(?i)mask protective") == T ~ "Surgical Mask",
			str_detect(product_name, "(?i)mask.*95") == T ~ "VN95 Mask",
			str_detect(product_name, "(?i)mueller.*broth") == T ~ "Mueller Hinton Broth",
			str_detect(product_name, "(?i)mueller.*agar") == T ~ "Mueller Hinton II Agar",
			str_detect(product_name, "(?i)needle 21g") == T ~ "Needle 21G",
			str_detect(product_name, "(?i)neutral red\\s+?(?!\\d)") == T ~ "Neutral Red",
			str_detect(product_name, "(?i)neutral red\\s+?\\d") == T ~ "Neutral Red 1%",
			str_detect(product_name, "(?i)petri dish") == T ~ "Petri dish",
			str_detect(product_name, "(?i)pipette.*3ml sterile") == T ~ "Pipette plastic 3ml sterile",
			str_detect(product_name, "(?i)pipette.*1000") == T ~ "Pipette Tip sterile 1000ul",
			str_detect(product_name, "(?i)pipette.*200") == T ~ "Pipette Tip sterile 200ul",
			str_detect(product_name, "(?i)kcl 3") == T ~ "Potassium chloride (KCl)",
			str_detect(product_name, "(?i)anti-bubble") == T ~ "Pourite anti-bubble additive",
			str_detect(product_name, "(?i)ribbon.*110mm x 300m.*") == T ~ "Ribbon 110x300m",
			str_detect(product_name, "(?i)ribbon.*27mm x 300m.*") == T ~ "Ribbon 27x300m",
			str_detect(product_name, "(?i)rubber stopper") == T ~ "Rubber stopper BHI 20mm",
			str_detect(product_name, "(?i)sabouraud.*glucose") == T ~ "Sabouraud 4% Glucose Agar",
			str_detect(product_name, "(?i)sabouraud.*chloram.*") == T ~ "Sabouraud Chloramphenicol Agar",
			str_detect(product_name, "(?i)plastic tube 12") == T ~ "Screw cap plastic tube 12ml",
			str_detect(product_name, "(?i)sim medium") == T ~ "SIM medium",
			str_detect(product_name, "(?i)simmon.*citrate") == T ~ "Simmons Citrate Agar",
			str_detect(product_name, "(?i)hand.*wash") == T ~ "Liquid hand soap",
			str_detect(product_name, "(?i)sodium chloride") == T ~ "Sodium chloride (NaCl)",
			str_detect(product_name, "(?i)sodium hydroxide") == T ~ "Sodium hydroxide (NaOH)",
			str_detect(product_name, "(?i)sodium polyane") == T ~ "Sodium Polyanethole Sulfate (SPS)",
			str_detect(product_name, "(?i)soyabean.*agar") == T ~ "Soyabean Casein Digest Agar (TSA)",
			str_detect(product_name, "(?i)soyabean.*medium") == T ~ "Soyabean Casein Digest Medium (TSB)",
			str_detect(product_name, "(?i)2.*cryovial") == T ~ "Sterile screw tube 2ml",
			str_detect(product_name, "(?i)gauze") == T ~ "Sterilized gauze",
			str_detect(product_name, "(?i)syringe 5") == T ~ "Syringe 5cc",
			str_detect(product_name, "(?i)syringe 10") == T ~ "Syringe 10cc",
			str_detect(product_name, "(?i)syringe 20") == T ~ "Syringe 20cc",
			str_detect(product_name, "(?i)thiosulfate citrate") == T ~ "TCBS",
			str_detect(product_name, "(?i)tetracycl") == T ~ "Tetracycline 30mg",
			str_detect(product_name, "(?i)triple packaging") == T ~ "Triple packaging",
			str_detect(product_name, "(?i)tryptic.* blood agar.*") == T ~ "Tryptic Soy Blood Agar Base No 2",
			str_detect(product_name, "(?i)tryptic.*broth") == T ~ "Tryptic Soy Broth",
			str_detect(product_name, "(?i)tryptic.*agar") == T ~ "Trypticase Soy Agar",
			str_detect(product_name, "(?i)urea agar") == T ~ "Urea agar base",
			str_detect(product_name, "(?i)urea.*40%") == T ~ "Urea supplement 40%",
			str_detect(product_name, "(?i)vcnt.*supplement") == T ~ "VCNT supplement",
			str_detect(product_name, "(?i)(vitamino.*supplement)|(vitox)") == T ~ "Vitamino supplement",
			str_detect(product_name, "(?i)kimcap.*13") == T ~ "KimCap 13mm",
			str_detect(product_name, "(?i)colomycin") == T ~ "colistin",
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
