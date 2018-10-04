# District
#Peguei o Script do docimento word.

#A tabela district contém informações da região que o cliente se localiza. Como as colunas estão codificadas, realizamos a alteração dos nomes para facilitar o entendimento:

colnames(district) <- c("dist_codigo", "dist_nome", "regiao", "habitantes", "hab_menor_499", "hab500_1999", "hab2000_9999", "hab_maior_10000", "cidades", "prop_urban_hab", "salário_medio_distrito", "Desemprego_95", "Desemprego_96", "empreendedor_1000_hab","num_crimes_95", "num_crimes_96")

#deletar a primeira linha, Esse eu que fiz :)
district <- district[-1, ]

#Formatação para numero (tive que adaptar do Script em word para funcionar)
district<- mutate(district, Desemprego_95 = as.double(Desemprego_95))
district<- mutate(district, num_crimes_95 = as.double(num_crimes_95))
district<- mutate(district, prop_urban_hab = as.double(prop_urban_hab))

#taxa rural
district<- mutate(district, Taxa_rural_hab = as.double(100 - prop_urban_hab))


#Valores médios entre taxa de desemprego e número de crimes

district_media<- district %>% group_by(dist_codigo)%>%
  summarise(Desemprego_medio = as.double(sum(Desemprego_95,Desemprego_96)/2),
            NumerodeCrimesmedio = as.double(sum(num_crimes_95,num_crimes_96)/2))

#Incluir a tabela district media na original, usando leftjoin.

district<- left_join(district,district_media, by = c("dist_codigo"="dist_codigo"))

#limpar
district_media <- NULL

