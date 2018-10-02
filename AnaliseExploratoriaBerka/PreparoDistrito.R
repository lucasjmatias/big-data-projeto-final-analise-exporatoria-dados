read.csv2("./dados/district.asc", stringsAsFactors = FALSE) -> district


#Renomear campos para melhor entendimento

colnames(district)[1] <- 'district_id'
colnames(district)[2] <- 'district_name'
colnames(district)[11] <- 'avg_sal'
colnames(district)[12] <- 'unemp_95'
colnames(district)[13] <- 'unemp_96'

#Converter campos para numérico
district$unemp_95 = as.numeric(district$unemp_95)
district$unemp_96 = as.numeric(district$unemp_96)

#Limpeza de NA
district[is.na(district$unemp_95),12] <- 1

  district %>%
  mutate(unemp_r = ifelse(unemp_95 == 0 | unemp_96 == 0, 1, unemp_96/unemp_95)) %>%
  select(district_id, district_name, avarage_salary, unemp_95, unemp_96) -> district

View(district)
