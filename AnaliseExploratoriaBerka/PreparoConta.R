read.csv2("./dados/account.asc", stringsAsFactors = FALSE) -> account


View(account)

#Tradução
account$frequency <- gsub("POPLATEK MESICNE", "mensal", account$frequency)
account$frequency <- gsub("POPLATEK TYDNE", "semanal", account$frequency)
account$frequency <- gsub("POPLATEK PO OBRATU", "acadatransacao", account$frequency)
account$frequency <- as.factor(account$frequency)
account <- select(account, account_id, district_id, frequency)

?gsub

