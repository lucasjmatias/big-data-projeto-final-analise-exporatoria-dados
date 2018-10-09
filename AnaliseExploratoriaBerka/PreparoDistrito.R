read.csv2("./dados/district.asc", stringsAsFactors = FALSE) -> district

prepararNa <- function (tabela) {
  function (nomeColuna, valorSeNA) {
    indiceDados <- which(colnames(tabela)==nomeColuna)
    tabela[is.na(tabela[,indiceDados]),indiceDados] <- valorSeNA
    return(tabela)
  }
}


#Renomear campos para melhor entendimento
colnames(district)[1] <- 'district_id'
colnames(district)[2] <- 'district_name'
colnames(district)[11] <- 'avg_sal'
colnames(district)[12] <- 'unemp_95'
colnames(district)[13] <- 'unemp_96'

#Converter campos para numérico
district$unemp_95 = as.numeric(district$unemp_95)
district$unemp_96 = as.numeric(district$unemp_96)

#indiceDados <- which(colnames(district)=="unemp_95")
#district[is.na(district[,indiceDados]),indiceDados] <- 1


#Limpeza de NA
#district[is.na(district$unemp_95),12] <- 1
district <- prepararNa(district)("unemp_95", 1)
#Cálculo da taxa de desemprego e seleção de valores
district %>%
  mutate(unemp_r = ifelse(unemp_95 == 0 | unemp_96 == 0, 1, unemp_96/unemp_95)) %>%
  select(district_id, district_name, avg_sal, unemp_r) -> district
