read.csv2("./dados/trans.asc", stringsAsFactors = FALSE) -> trans

unique(trans$type)

trans$type <- gsub("PRIJEM", "credito", trans$type)
trans$type <- gsub("VYDAJ", "debito", trans$type)


trans <- trans %>%
        dplyr::filter(type != "VYBER") %>%
        mutate(amount = as.numeric(amount)) %>%
        select(account_id, type, amount) %>%
        group_by(account_id) %>%
        summarise(
          credito = sum(ifelse(type == "credito", amount, 0)),
          debito = sum(ifelse(type == "debito", amount, 0)),
          count = n()) %>%
        mutate(withdraw_rate = debito / credito) %>%
        select(account_id, withdraw_rate, count)

View(trans)
