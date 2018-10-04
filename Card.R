#Card

#transformar Issued em data

Card$year <- c(1900 + as.numeric(substr(card$issued, 1,2)))
card$month <- c(as.numeric(substr(card$issued, 3,4)))
card$day <- c(as.numeric(substr(card$issued, 5,6)))
card$data_issued_card <- as.Date(with(card, paste(year, month, day, sep = "-")),"%Y-%m-%d")

#Limpar
card$year <- NULL
card$month <- NULL
card$day <- NULL
card$issued <- NULL

#Criar DummY

card <- mutate(card, card_classic = ifelse(type == "classic", 1,0))
card <- mutate(card, card_gold = ifelse(type == "gold", 1,0))
card <- mutate(card, card_junior = ifelse(type == "junior", 1,0))

