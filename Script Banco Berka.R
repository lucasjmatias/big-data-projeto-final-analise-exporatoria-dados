#Importar os dados dentro do R (depende onde o arquivo esta)

# Obs: Banco de dados Trans é pesado.

account <- read.csv("C:/Users/cesar/Desktop/Banco Berka/account.asc", sep=";")
View(account)
card <- read.csv("C:/Users/cesar/Desktop/Banco Berka/card.asc", sep=";")
View(card)
client <- read.csv("C:/Users/cesar/Desktop/Banco Berka/client.asc", sep=";")
View(client)
disp <- read.csv("C:/Users/cesar/Desktop/Banco Berka/disp.asc", sep=";")
View(disp)
district <- read.csv("C:/Users/cesar/Desktop/Banco Berka/district.asc", header=FALSE, sep=";")
View(district)
loan <- read.csv("C:/Users/cesar/Desktop/Banco Berka/loan.asc", sep=";")
View(loan)
order <- read.csv("C:/Users/cesar/Desktop/Banco Berka/order.asc", sep=";")
View(order)
trans <- read.csv("C:/Users/cesar/Desktop/Banco Berka/trans.asc", sep=";")
View(trans)

# Pacotes:

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(scales)
library(ggmap)


#limpeza dos dados

#Banco card: 
#Variavel Dummie: Type, devemos converte-la em factor:
card$type <- as.factor(card$type)

#Arrumar a data

card$issued <- as.Date(paste(card$issued), "%y%m%d")

#banco Client:

#Arrumar a data de nascimento


#disp:

#Transformar a a coluna type em dummie

disp$type <- as.factor(disp$type)


#Gráfico de quatidades:
p1 <- ggplot(disp, aes(x = type))
p1 <- p1 + geom_bar(aes(y = (..count..)/sum(..count..),fill=type))
p1 + scale_y_continuous("percent")


#account:
  
#1. date: preparar para data, atualmente está em "YYMMDD".

account$date <- as.Date(paste(account$date), "%y%m%d")

#2. frequency: Traduzir status para inglês ou português.

names(account)[3] <- paste("tipoextratos")

account$tipoextratos <- gsub("POPLATEK MESICNE", "mensal", account$tipoextratos)
account$tipoextratos <- gsub("POPLATEK TYDNE", "semanal", account$tipoextratos)
account$tipoextratos <- gsub("POPLATEK PO OBRATU", "acadatransacao", account$tipoextratos)
account$tipoextratos <- as.factor(account$tipoextratos)

#client:
  
#birth: Valores são YYMMDD e YYMM+50DD, onde +50DD representa o sexo femínino. Formatar data e separar o campo de gênero, M e F.

client <- client %>%
  mutate(mesajustado = as.numeric(stringr::str_sub(birth_number,3,4))) %>%
  mutate(sex = ifelse(mesajustado > 50, "F", "M")) %>%
  mutate(birth_number = ifelse(sex=="F", birth_number - 5000, birth_number))
client$birth_number <- paste0("19", client$birth_number)
client$birth_number <- as.Date(client$birth_number, "%Y%m%d")



