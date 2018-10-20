View(dados)

summary(dados)


?as.factor

#Existe relação entre valor do empréstimo e problemas com empréstimo
boxplot(loan_amount~problemas_loan, data = dadosLoan)
lm(dadosLoan, formula = loan_amount ~ problemas_loan) -> modelo
summary(modelo)

boxplot(saldo_medio_em_conta~problemas_loan, data = dadosLoan)
plot(saldo_medio_em_conta~problemas_loan, data = dadosLoan)
lm(dadosLoan, formula = saldo_medio_em_conta ~ problemas_loan) -> modelo2
summary(modelo2)


lm(dadosLoan, formula = loan_payment_rate ~ problemas_loan) -> modelo
summary(modelo)
boxplot(loan_payment_rate~problemas_loan, data = dadosLoan)




pairs(select(dadosLoan, problemas_loan, loan_amount, age, saldo_medio_em_conta, withdraw_rate, quant_trans, loan_payment_rate))

pairs(select(dadosLoan, problemas_loan, saldo_medio_em_conta))

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



hist(dados$saldo_medio_em_conta)

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

dadosLoan$problemas_loan = as.double(dadosLoan$problemas_loan)
dadosLoan$ja_pagou_seguro = as.double(dadosLoan$ja_pagou_seguro)
dadosLoan$paga_divida = as.double(dadosLoan$paga_divida)
dadosLoan$paga_leasing = as.double(dadosLoan$paga_leasing)
dadosLoan$has_card = as.double(dadosLoan$has_card)
View(dadosLoan)
str(dadosLoan)


dadosLoan2$ja_pagou_seguro = as.double(dadosLoan2$ja_pagou_seguro)
lm(formula = problemas_loan ~ has_card + 
     age + 
     loan_age + account_age + loan_duration + loan_amount + loan_payment_rate +
     paga_leasing +
     ja_pagou_seguro + avg_sal + quant_trans + media_transf + numb_enter +
     quant_ordem + total_ordem + unemp_r + saldo_medio_em_conta, data = dadosLoan) -> modelo
summary(modelo)

ggplot(data = dadosLoan) +  geom_bar(mapping = aes(x = problemas_loan, fill=has_card))


ggplot(data = dadosLoan, mapping = aes(x = problemas_loan, color = has_card)) +  geom_freqpoly(binwidth = 0.1)



ggplot(data = dadosLoan, mapping = aes(x = avg_sal, y = saldo_medio_em_conta)) +  geom_point() 




lm(formula = problemas_loan ~ saldo_medio_em_conta, data = dadosLoan) -> modelo

lm(formula = problemas_loan ~ account_age + loan_duration + loan_amount +
     ja_pagou_seguro + no_account_users +
     quant_ordem + saldo_medio_em_conta + media_transf + quant_trans + trans_amount, data = dadosLoan) -> modelo


str(dadosLoan)
dadosLoan2 <- select(dadosLoan,  age,  avg_sal,  unemp_r,  account_age,  total_ordem, 
                     quant_ordem,  ja_pagou_seguro,  media_transf,  withdraw_rate,
                     quant_trans,  saldo_medio_em_conta,  loan_id,  loan_amount,  loan_duration,  loan_payment_rate,
                     loan_age,  problemas_loan,  has_card)

dadosLoan2 <- select(dadosLoan,problemas_loan, age, loan_amount, account_age, loan_duration, ja_pagou_seguro, trans_amount, numb_enter, avg_sal, unemp_r, no_account_users, saldo_medio_em_conta)
lm(formula = problemas_loan ~ ., data = dadosLoan2) -> modelo

dadosLoan2 <- select(dados, age, loan_amount, account_age, trans_amount, numb_enter, avg_sal, unemp_r)

summary(modelo)



lm(formula = problemas_loan ~ has_card + 
     age + 
     loan_age + account_age + loan_duration + loan_amount + loan_payment_rate +
     paga_leasing +
     ja_pagou_seguro + avg_sal + quant_trans + media_transf + numb_enter +
     quant_ordem  + saldo_medio_em_conta, data = dadosLoan) -> modelo
summary(modelo)

str(dadosLoan2)
loanScale <- as.matrix(scale(dadosLoan2))
boxplot(loanScale)
View(loanScale)

fitLoan <- kmeans(loanScale, 3)
fitLoan
plot(loanScale, col=fitLoan$cluster, pch=15)

clusplot(loanScale, fitLoan$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)



lm(formula = problemas_loan ~ loan_duration + loan_amount +
     ja_pagou_seguro + no_account_users +
     quant_ordem + saldo_medio_em_conta + media_transf + quant_trans + trans_amount, data = dadosLoan) -> modelo
str(dadosL)
dadosLoan$problemas_loan = as.double(dadosLoan$problemas_loan)
dadosLoan$ja_pagou_seguro = as.double(dadosLoan$ja_pagou_seguro)
dadosLoan$paga_divida = as.double(dadosLoan$paga_divida)
dadosLoan$paga_leasing = as.double(dadosLoan$paga_leasing)
dadosLoan$has_card = as.double(dadosLoan$has_card)


dadosLoan2 <- dadosLoan %>%
              select( problemas_loan, ja_pagou_seguro, no_account_users, fq_saldo, media_transf,
                      quant_ordem, min_saldo)

lm(formula = problemas_loan ~ ., data = dadosLoan2) -> modelo
summary(modelo)

modelo

plot(dadosLoan2, col = dadosLoan2$problemas_loan)

boxplot(dadosLoan2$fq_saldo)
summary(dadosLoan2$mediana_saldo)[2]

cor(dadosLoan2)
str(dadosLoan2[-1])
str(dadosLoan2)
dadosLoan2_std <- scale(dadosLoan2[-1])
fit_km <- kmeans(dadosLoan2_std, 4, nstart = 50,iter.max = 15)
fit_km
fit_km$centers
plot(dadosLoan2, col = fit_km$cluster)

plot(fit_km$cluster)

lm(formula = fit_km$cluster ~ dadosLoan2$ja_pagou_seguro) -> modelo2
summary(modelo2)


?kmeans


scaled_data = as.matrix(dadosLoan2_std)


set.seed(123)
k.max <- 15
data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")




plot(dadosLoan2, col = kmeans$cluster)



lm(formula = problemas_loan ~ ., data = dadosLoan2) -> modelo


plot(dadosLoan2$problemas_loan ~ dadosLoan2, col = region)

ggplot(dadosLoan, aes(x = region)) + geom_bar(stat = "bin", position=position_dodge(), aes(fill=problemas_loan))

distritoAnalise <- dadosLoan %>% 
                   select(region, problemas_loan) %>%
                   group_by(region, problemas_loan) %>%
                   summarise(
                     count = n()
                   ) %>%
                   mutate(perc =  count/sum(count))


ggplot(distritoAnalise, aes(x = factor(region), y = perc*100, fill = factor(problemas_loan))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Região", y = "percent", fill = "Problema com pagamento") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
dadosLoan$problemas_loan = as.double(dadosLoan$problemas_loan)
ggplot(dadosLoan, aes(region)) + geom_bar(aes(fill=problemas_loan)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



analiseCartao <- dadosLoan %>% 
  select(card_type, problemas_loan) %>%
  group_by(card_type, problemas_loan) %>%
  summarise(
    count = n()
  ) %>%
  mutate(perc =  count/sum(count))


ggplot(analiseCartao, aes(x = factor(card_type), y = perc*100, fill = factor(problemas_loan))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Região", y = "Porcentagem", fill = "Problema com pagamento") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




analiseDivida <- dadosLoan %>% 
  mutate(ano = year(loan_date)) %>%
  select(ano, problemas_loan) %>%
  group_by(ano, problemas_loan) %>%
  summarise(
    count = n()
  ) %>%
  mutate(perc =  count/sum(count))


ggplot(analiseDivida, aes(x = factor(ano), y = perc*100, fill = factor(problemas_loan))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Ano", y = "Porcentagem", fill = "Problema com pagamento") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(analiseDivida, aes(x = factor(ano), y = perc*100), group = 1) +
  geom_line() +
  labs(x = "Ano", y = "Porcentagem", fill = "Problema com pagamento") +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


analiseDivida <- dadosLoan %>% 
  mutate(ano = year(loan_date)) %>%
  select(ano, problemas_loan) %>%
  group_by(ano, problemas_loan) %>%
  summarise(
    count = n()
  ) %>%
  mutate(perc =  count/sum(count)) %>%
  dplyr::filter(problemas_loan == TRUE)

ggplot(analiseDivida, aes(x = ano, y = perc*100), group = 1) +
  geom_line(color="red") +
  geom_point(color="red") + 
  labs(x = "Ano", y = "Porcentagem de problemas com empréstimo") +
  expand_limits(y=c(0, 100))
  
