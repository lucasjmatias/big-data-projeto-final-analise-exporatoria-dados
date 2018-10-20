read.csv2("./dados/loan.asc", stringsAsFactors = FALSE) -> loan

unique(loan$status)
loan$status <- gsub("A", "finalizado", loan$status)
loan$status <- gsub("B", "nao pago", loan$status)
loan$status <- gsub("C", "vigente", loan$status)
loan$status <- gsub("D", "em debito", loan$status)
loan$payments = as.numeric(loan$payments)
loan$amount = as.numeric(loan$amount)

loan$loan_date = prepararData(loan$date)

loan2 <- loan %>%
  mutate(loan_year = year(loan_date)) %>%
  mutate(loan_end_year = loan_year + (duration / 12)) %>%
  dplyr::filter(loan_end_year > 1997) %>%
  mutate(loan_age = year(currentdate) - loan_year) %>%
  mutate(payments_rate = payments / amount) %>%
  select(loan_id, account_id, amount, duration, status, payments_rate, loan_date, loan_age)

View(loan2)

colnames(loan)[3] <- 'loan_amount'
colnames(loan)[4] <- 'loan_duration'
colnames(loan)[5] <- 'loan_status'
colnames(loan)[6] <- 'loan_payment_rate'