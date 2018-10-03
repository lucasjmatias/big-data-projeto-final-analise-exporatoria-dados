
#Carregar os dados
read.csv2("./dados/account.asc", stringsAsFactors = FALSE) -> account
read.csv2("./dados/client.asc", stringsAsFactors = FALSE) -> client
read.csv2("./dados/card.asc", stringsAsFactors = FALSE) -> card
read.csv2("./dados/disp.asc", stringsAsFactors = FALSE) -> disp
read.csv2("./dados/district.asc", stringsAsFactors = FALSE) -> district
read.csv2("./dados/trans.asc", stringsAsFactors = FALSE) -> trans
read.csv2("./dados/loan.asc", stringsAsFactors = FALSE) -> loan
read.csv2("./dados/order.asc", stringsAsFactors = FALSE) -> order




#account:

#1. date: preparar para data, atualmente está em "YYMMDD".

account$date <- as.Date(paste(account$date), "%y%m%d")

#2. frequency: Traduzir status para inglês ou português.

names(account)[3] <- paste("tipoextratos")

account$tipoextratos <- gsub("POPLATEK MESICNE", "mensal", account$tipoextratos)
account$tipoextratos <- gsub("POPLATEK TYDNE", "semanal", account$tipoextratos)
account$tipoextratos <- gsub("POPLATEK PO OBRATU", "acadatransacao", account$tipoextratos)
account$tipoextratos <- as.factor(account$tipoextratos)

View(account)

#client:

#birth: Valores são YYMMDD e YYMM+50DD, onde +50DD representa o sexo femínino. Formatar data e separar o campo de gênero, M e F.
currentdate <- as.Date("1998/01/01", format="%Y/%m/%d")
client <- client %>%
  mutate(mesajustado = as.numeric(stringr::str_sub(birth_number,3,4))) %>%
  mutate(sex = ifelse(mesajustado > 50, "F", "M")) %>%
  mutate(birth_number = ifelse(sex=="F", birth_number - 5000, birth_number)) %>%
  mutate(birth_number = paste0("19", birth_number)) %>%
  mutate(birth_number = as.Date(birth_number, "%Y%m%d")) %>%
  mutate(age = year(currentdate) - year(birth_number)) %>%
  select(client_id, age, birth_number, district_id, sex)

?select

#client$birth_number <- paste0("19", client$birth_number)
#client$birth_number <- as.Date(client$birth_number, "%Y%m%d")
View(client)

#Card

#Banco card: 
#Variavel Dummie: Type, devemos converte-la em factor:
card$type <- as.factor(card$type)
#Arrumar a data
card$issued <- as.Date(paste(card$issued), "%y%m%d")
View(card)


#disp:
#Transformar a a coluna type em dummie
disp$type <- as.factor(disp$type)
View(disp)


#Converter campos para numérico
district$A12 = as.numeric(district$A12)
district$A13 = as.numeric(district$A13)

#Limpeza de NA
district[is.na(district$A12),12] <- 1

str(district)
mutate(district, avg_sal = A11) %>%
  mutate(unemp_r = ifelse(A13 == 0 | A12 == 0, 1, A13/A12)) -> district_dados
View(district_dados)

View(district)
?mean

#preparação da coluna TrANS agrupada por conta
View(trans)
trans$amount = as.numeric(trans$amount)
aggregate(x = trans$amount, by = list(account_id=trans$account_id), FUN=sum) %>%
  mutate(amount=x) %>%
  select(account_id, amount) ->
  transSomaValorPorConta
View(transSomaValorPorConta)


