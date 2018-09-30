read.csv2("./dados/client.asc", stringsAsFactors = FALSE) -> client
read.csv2("./dados/account.asc", stringsAsFactors = FALSE) -> account
read.csv2("./dados/card.asc", stringsAsFactors = FALSE) -> card
read.csv2("./dados/disp.asc", stringsAsFactors = FALSE) -> disp
read.csv2("./dados/district.asc", stringsAsFactors = FALSE) -> district
read.csv2("./dados/loan.asc", stringsAsFactors = FALSE) -> loan
read.csv2("./dados/order.asc", stringsAsFactors = FALSE) -> order
read.csv2("./dados/trans.asc", stringsAsFactors = FALSE) -> trans


View(account)
bar(account$frequency)
?boxplot
ggplot(account, aes(frequency)) + geom_bar()
View(loan)



ggplot(loan, aes(x=status, y = "", fill=duration)) + geom_bar(stat = "identity")

hist(loan$amount)
oneway.test(loan, formula=amount~status)

lm(loan, formula=amount~status) -> modelo1
summary(modelo1)
boxplot(loan$amount~loan$status)
boxplot(loan$payments)

View(trans)
View(card)

str(loan)

ggplot(card, aes(type)) + geom_bar()

hist(card$type)
