read.csv2("./dados/account.asc", stringsAsFactors = FALSE) -> account


View(account)

#Tradução
account$frequency <- gsub("POPLATEK MESICNE", "mensal", account$frequency)
account$frequency <- gsub("POPLATEK TYDNE", "semanal", account$frequency)
account$frequency <- gsub("POPLATEK PO OBRATU", "acadatransacao", account$frequency)
account$frequency <- as.factor(account$frequency)

account <- account %>%
  mutate(account_date = paste0("19", date)) %>%
  mutate(account_date = as.Date(account_date, "%Y%m%d")) %>%
  select(account_id, frequency, account_date)

?gsub

