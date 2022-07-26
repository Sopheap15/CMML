# IQC----
# raw data from system and filter to date > 2021-01-01

iqc <- list.files("data", pattern = "^(IQC|iqc).*.csv", full.names = T) %>%
	read_csv2(trim_ws = T, na = c("","NA")) %>% 
	mutate(qc_date = as.Date(qc_date, "%d/%m/%y")) %>% 
	filter(qc_date >= date[1,2], 
				 qc_date <= date[2,2], 
				 qc_result != "", !batch_no %in% c("	
KIA260521-01 ", "KIA260521-01", "KIA030621-01", "KIA090621-01")) %>%
	mutate(exp = str_extract(comments,"[Ee]xper.*"),
				 qc_result = case_when(qc_result == 0 ~ "yes",
				 											qc_result == 1 ~ "no", 
				 											TRUE ~ "unknow")
				 
				 )