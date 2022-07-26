# Save data of DMDP support site to excel----
data %>% 
	filter(delivery_date >= date[3, 2], delivery_date < date[4, 2], group == "Govt. Lab (supported by DMDP)") %>%
	group_by(customer, media_name, d_month) %>% 
	summarise(n = sum(d_quantity),
						'Cost (USD)' = round_half_up(sum(cost)),.groups = "drop") %>%
	arrange(d_month) %>% 
	mutate(d_month = factor(d_month, levels = month.abb)) %>% 
	pivot_wider(names_from = d_month, values_from = n) %>% 
	arrange(customer) %>% 
	adorn_totals("col",,,, -'Cost (USD)', name = 'Total quan. (unit)') %>% 
	relocate('Cost (USD)', .after = 'Total quan. (unit)') %>% 
	mutate_all(~replace(., is.na(.), 0)) %>% 
	group_by(customer, media_name) %>% 
	summarise(across(everything(), sum)) %>% 
	export("Output/Media distributed to DMDP supported site.xlsx", 
				 overwrite = T,
				 asTable = T)