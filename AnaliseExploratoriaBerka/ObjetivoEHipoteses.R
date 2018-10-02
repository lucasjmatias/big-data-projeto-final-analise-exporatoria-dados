---
title: "Análise de dados - Banco Czech"
output: pdf_document
---
  


#Carregar os dados
read.csv2("./dados/account.asc", stringsAsFactors = FALSE) -> account
read.csv2("./dados/client.asc", stringsAsFactors = FALSE) -> client
read.csv2("./dados/card.asc", stringsAsFactors = FALSE) -> card
read.csv2("./dados/disp.asc", stringsAsFactors = FALSE) -> disp
read.csv2("./dados/district.asc", stringsAsFactors = FALSE) -> district
read.csv2("./dados/trans.asc", stringsAsFactors = FALSE) -> trans


install.packages("rmarkdown")

#O objetivo do nosso trabalho é identificar quais fatores que podem impactar no atraso e não pagamento das dívidas. 

#
#
#
