read.csv2("./dados/card.asc", stringsAsFactors = FALSE) -> card

#Card

#Banco card: 
#Variavel Dummie: Type, devemos converte-la em factor:
card$type <- as.factor(card$type)
#Arrumar a data
card <- select(card, card_id, disp_id, type)


colnames(card)[3] <- 'card_type'
card <- card %>%
  select(card_id, disp_id, card_type) %>%
  mutate(card_type = as.factor(card_type))

View(card)
card
summary(card)
