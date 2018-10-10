View(dados)

summary(dados)


?as.factor

#Existe relação entre valor do empréstimo e problemas com empréstimo
boxplot(loan_amount~problemas_loan, data = dadosLoan)
lm(dadosLoan, formula = loan_amount ~ problemas_loan) -> modelo
summary(modelo)

boxplot(saldo_medio_em_conta~problemas_loan, data = dadosLoan)
lm(dadosLoan, formula = saldo_medio_em_conta ~ problemas_loan) -> modelo2
summary(modelo2)


lm(dadosLoan, formula = loan_payment_rate ~ problemas_loan) -> modelo
summary(modelo)
boxplot(loan_payment_rate~problemas_loan, data = dadosLoan)




pairs(select(dadosLoan, problemas_loan, loan_amount, age, saldo_medio_em_conta, withdraw_rate, quant_trans, loan_payment_rate))

pairs(select(dadosLoan, problemas_loan, loan_payment_rate))

dadosLoan 


View(dadosLoan)

str(dados)

dados <- fastDummies::dummy_cols(dados, select_columns = "loan_status")

ggplot(dados, aes(x=loan_status, y = loan_amount)) + boxplot()

boxplot(loan_amount~loan_status, data = dadosLoan)


boxplot(dados$withdraw_rate ~ dados$loan_amount)
dados$loan_status

chisq.test(table(dados$card_type, dados$loan_status))
chisq.test(table(dados$loan_status, dados$gender))
chisq.test(table(dados$loan_status, dados$frequency))
chisq.test(table(dados$card_type, dados$frequency))

oneway.test(dados$unemp_r ~ dados$loan_status)




ggplot(dados, aes(card_type)) + geom_bar(position=position_dodge(), aes(fill=frequency))

ggplot(dados, aes(x = loan_status, y = loan_amount)) + geom_bar(stat = "identity", position=position_dodge(), aes(fill=card_type))


?chisq.test



hist(dados$loan_amount)

lm(dados, formula = u ~ loan_amount) -> modelo

lm(dados, formula = loan_duration ~ loan_amount) -> modelo


summary(modelo)



oneway.test(dados$loan_amount ~ dados$district_name)
lm(dados, formula = loan_amount ~ cri) -> modelo
summary(modelo)


boxplot(dados$age ~ dados$card_type)

oneway.test(dados, formula = age ~ loan_status)

hist(dados$loan_amount)


hist(dados, formula = loan_status ~ ja_pagou_seguro)

?lm


dadosLoan <- dadosLoan %>%
               mutate(loan_year = year(loan_date))

dadosLoan95$problemas_loan <- as.factor(dadosLoan95$problemas_loan)
boxplot(dadosLoan95$problemas_loan)

hist(dadosLoan95$problemas_loan)

boxplot(loan_date~problemas_loan, data = dadosLoan)

?boxplot

ggplot(dadosLoan, aes(x = loan_date, y = problemas_loan)) + geom_bar(stat = "identity", position=position_dodge(), aes(fill=card_type))


anoPorProblemas = dadosLoan %>%
                  mutate(loan_year = year(loan_date)) %>%
                  group_by(loan_year) %>%
                  summarise(
                    count = n(),
                    problemas = sum(ifelse(problemas_loan == TRUE, 1, 0))
                  ) %>%
                  mutate(taxa_problema = problemas / count)

boxplot(taxa_problema~loan_year, data = anoPorProblemas)

anoPorProblemas$loan_year = as.factor(anoPorProblemas$loan_year)
ggplot(anoPorProblemas, aes(x = loan_year, y = count)) +
  geom_bar(stat = "identity", position=position_dodge(), aes(fill=taxa_problema))


View(anoPorProblemas)

oneway.test(dados, formula = age ~ loan_status)

dadosLoan <- dadosLoan %>%
  mutate(has_card = ifelse(card_type=="nenhum", FALSE, TRUE))

chisq.test(table(dadosLoan$has_card, dadosLoan$problemas_loan))
ggplot(dadosLoan, aes(has_card)) +
  geom_bar(position=position_dodge(), aes(fill=problemas_loan))

?glm


chisq.test(table(dadosLoan$unemp_r, dadosLoan$problemas_loan))

oneway.test(dadosLoan, formula = avg_sal ~ problemas_loan)
str(dadosLoan)
pairs(select(dadosLoan, problemas_loan, avg_sal, unemp_r, saldo_medio_em_conta, loan_amount))

dadosLoan$problemas_loan = as.factor(dadosLoan$problemas_loan)
glm(formula = problemas_loan ~ has_card, data = dadosLoan) -> modelo
