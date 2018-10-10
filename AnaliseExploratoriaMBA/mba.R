library(stats)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RODBC)


read.csv2("mba.csv", stringsAsFactors = FALSE) -> mba
mean(mba$salary)
median(mba$salary)
hist(mba$salary)
filter(mba, salary!= 999 & salary != 998 ) -> mba2

hist(mba2$salary)
filter(mba2, salary != 0) -> mba3
hist(mba3$salary)
View(mba3)


mean(mba3$salary)
median(mba3$salary)

oneway.test(mba3, formula=salary~sex)

boxplot(mba3$salary ~ mba3$sex)

filter(mba3, salary != max(salary)) -> mba4
oneway.test(mba4, formula=salary~sex)
boxplot(mba4$salary ~ mba4$sex)
hist(mba4$salary)
View(mba4)

oneway.test(mba4$salary ~ mba4$quarter)
boxplot(mba4$salary ~ mba4$quarter)

oneway.test(mba3$salary ~ mba3$quarter)

lm(mba4$salary ~ mba4$quarter)

lm(mba3$salary ~ mba3$quarter)

lm(mba4$salary ~ mba4$gmat_tot) -> modelo1

summary(modelo1)

oneway.test(mba4$salary ~ mba4$frstlang)
unique(mba4$frstlang)
lm(mba4$salary ~ mba4$frstlang) -> modelo2
summary

sum(mba4$frstlang == 1)
table(mba4$frstlang)

lm(mba4, formula = salary ~ age) -> modelo2
summary(modelo2)

lm(mba4, formula = salary ~ work_yrs) -> modelo3
summary(modelo3)

lm(mba4, formula = salary ~ age + work_yrs) -> modelo4
summary(modelo4)

lm(mba4, formula = salary ~ sex + quarter + work_yrs) -> modelo5
modelo5
summary(modelo5)
119309.8 - 9973.4 * 2 - 3462.8 * 1 +  912.2 * 8


