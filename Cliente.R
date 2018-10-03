#client:

#birth: Valores são YYMMDD e YYMM+50DD, onde +50DD representa o sexo femínino. Formatar data e separar o campo de gênero, M e F.

client <- mutate(client, sex = ifelse(substr(client$birth_number, 3,3) == 5 | substr(client$birth_number, 3,3) == 6, "F", "M"))

client <- mutate(client, birth_year = as.numeric(substr(client$birth_number, 1,2)))

client <- mutate(client, age = as.double(1998 - (1900 + client$birth_year)))
