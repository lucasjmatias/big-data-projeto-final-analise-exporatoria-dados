read.csv2("./dados/loan.asc", stringsAsFactors = FALSE) -> loan

unique(loan$status)
loan$status <- gsub("A", "finalizado", loan$status)
loan$status <- gsub("B", "finalizado nao pago", loan$status)
loan$status <- gsub("C", "vigente", loan$status)
loan$status <- gsub("D", "em debito", loan$status)
loan$status <- as.factor(loan$status)
loan$payments = as.numeric(loan$payments)
loan$amount = as.numeric(loan$amount)

loan <- loan %>%
        mutate(payments_rate = payments / amount) %>%
        select(loan_id, account_id, amount, duration, status, payments_rate)


View(loan)

