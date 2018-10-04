
str(client)
str(disp)
str(card)
str(district)
str(account)
str(order)
str(trans)

str(dados)
str(loan)

dados <- client %>%
         inner_join(disp, by = "client_id") %>%
         left_join(card, by = "disp_id") %>%
         left_join(district, by = "district_id") %>%
         inner_join(account, by = "account_id") %>%
         left_join(order, by= "account_id") %>%
         left_join(trans, by="account_id") %>%
         inner_join(loan, by="account_id")
         
    

View(dados)
