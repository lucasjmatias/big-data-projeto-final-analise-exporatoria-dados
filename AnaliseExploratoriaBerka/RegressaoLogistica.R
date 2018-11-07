install.packages("popbio")
#library(aod)

library(popbio)

mylogit <- glm(problemas_loan ~ min_saldo, data = dadosLoanCluster, family = "binomial")
summary(mylogit)
mylogit



ggplot(dadosLoanCluster, aes(x = problemas_loan, y = min_saldo)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = 0.2) +
  geom_line(aes(colour = rank), size = 1)


plot(dadosLoanCluster$problemas_loan ~ dadosLoanCluster$min_saldo)

plot(dadosLoanCluster$min_saldo, dadosLoanCluster$problemas_loan)

min_saldo <- sort(dadosLoanCluster$min_saldo)
min_saldo
logi.hist.plot(min_saldo,dadosLoanCluster$problemas_loan,boxp=FALSE,type="hist",col="gray")

dadosLoan$min_saldo <- as.vector(dadosLoan$min_saldo)
mylogit <- glm(problemas_loan ~ min_saldo, data = dadosLoan, family = "binomial")
newdat <- data.frame(min_saldo=seq(min(dadosLoan$min_saldo), max(dadosLoan$min_saldo),len=682))
newdat$problemas_loan = predict(mylogit, newdata=newdat, type="response")
plot(problemas_loan~min_saldo, data=dadosLoan, col="red4")
lines(problemas_loan ~ min_saldo, newdat, col="green4", lwd=2)



dadosLoanCluster$min_saldo <- as.vector(dadosLoanCluster$min_saldo)
fit = glm(vs ~ hp, data=mtcars, family=binomial)
newdat <- data.frame(hp=seq(min(mtcars$hp), max(mtcars$hp),len=100))
newdat$vs = predict(fit, newdata=newdat, type="response")
plot(vs~hp, data=mtcars, col="red4")
lines(vs ~ hp, newdat, col="green4", lwd=2)

str(as.vector(dadosLoanCluster$min_saldo))
dadosLoanCluster$min_saldo[,1]

summary(dadosLoanCluster$min_saldo)

apply(dadosLoanCluster$min_saldo)
