View(dados)

summary(dados)


dados[is.na(dados$card_type),8] <- 'nenhum'
dados[is.na(dados$total_ordem),13] <- 0
dados[is.na(dados$quant_ordem),14] <- 0
dados[is.na(dados$ja_pagou_seguro),15] <- FALSE
dados[is.na(dados$media_transf),16] <- 0
dados[is.na(dados$loan_amount),20] <- 0
dados[is.na(dados$loan_duration),21] <- 0
dados[is.na(dados$loan_status),22] <- 'nenhum'



dados$card_type = as.factor(dados$card_type)
dados$loan_status = as.factor(dados$loan_status)


View(dados)

str(dados)

dados <- fastDummies::dummy_cols(dados, select_columns = "loan_status")

ggplot(dados, aes(x=loan_status, y = loan_amount)) + boxplot()

boxplot(loan_amount~loan_status, data = dados)


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
