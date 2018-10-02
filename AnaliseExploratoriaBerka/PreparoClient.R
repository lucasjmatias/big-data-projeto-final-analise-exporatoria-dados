read.csv2("./dados/client.asc", stringsAsFactors = FALSE) -> client
library(lubridate)


#birth: Valores são YYMMDD e YYMM+50DD, onde +50DD representa o sexo femínino. Formatar data e separar o campo de gênero, M e F.
client <- client %>%
  mutate(mesajustado = as.numeric(stringr::str_sub(birth_number,3,4))) %>%
  mutate(gender = ifelse(mesajustado > 50, "F", "M")) %>%
  mutate(birth_number = ifelse(gender=="F", birth_number - 5000, birth_number)) %>%
  mutate(birth_number = paste0("19", birth_number)) %>%
  mutate(birth_number = as.Date(birth_number, "%Y%m%d")) %>%
  mutate(age = year(currentdate) - year(birth_number)) %>%
  select(client_id, age, district_id, birth_number, gender)
  
View(client)
