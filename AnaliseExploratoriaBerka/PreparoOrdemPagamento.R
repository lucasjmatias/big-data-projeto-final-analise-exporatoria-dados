read.csv2("./dados/order.asc", stringsAsFactors = FALSE) -> order

unique(order$k_symbol)
order$k_symbol = gsub("POJISTNE", "seguro", order$k_symbol)
order$k_symbol = gsub("SIPO", "domestico", order$k_symbol)
order$k_symbol = gsub("LEASING", "leasing", order$k_symbol)
order$k_symbol = gsub("UVER", "divida", order$k_symbol)
order$amount = as.numeric(order$amount)

order <- order %>%
         dplyr::filter(k_symbol != " ") %>%
         group_by(account_id) %>%
         summarise(
            total_amount = sum(amount),
            quant = n(),
            ja_pagou_seguro = any(k_symbol == "seguro"),
            media_transf = sum(amount) / n()
         )

View(order)
