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
