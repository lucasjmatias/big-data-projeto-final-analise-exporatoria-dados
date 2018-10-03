read.csv2("./dados/disp.asc", stringsAsFactors = FALSE) -> disp

View(disp)
#disp:
#Transformar a a coluna type em dummie
disp$type <- as.factor(disp$type)
View(disp)

unique(disp$type)
disp <- disp %>%
        dplyr::filter(type == "OWNER") %>%
        select(disp_id, client_id, account_id)

View(disp)        
        