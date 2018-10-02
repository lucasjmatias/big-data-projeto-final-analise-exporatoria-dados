read.csv2("./dados/card.asc", stringsAsFactors = FALSE) -> card

#Card

#Banco card: 
#Variavel Dummie: Type, devemos converte-la em factor:
card$type <- as.factor(card$type)
#Arrumar a data
card <- select(card, card_id, disp_id, type)


card <- card %>%
        select(card_id, disp_id, type) %>%
        mutate(type = as.factor(type)) 

View(card)
card
