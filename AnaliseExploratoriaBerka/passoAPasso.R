#install.packages("RODBC")
#install.packages("magrittr")
#install.packages("cluster")
install.packages("fpc")

mclust 

library(RODBC)
library(magrittr)
library(cluster)



read.csv2("./dados/analise_cluster.csv", stringsAsFactors = FALSE) -> rawdata

#conn <- odbcConnectExcel2007(xls.file = "./dados/analise_cluster.xlsx")
#rawdata <- sqlFetch("analise_cluster")
#odbcClose(conn)


View(rawdata)
summary(rawdata)
numdata <- rawdata[,c(3,6:10)]
View(numdata)
numdata$unemp_r
?is.na
#Tratamento de dados - eliminação de NAS
numdata[is.na(numdata[,5]),5] <- 1
numdata[is.na(numdata[,5]),5]
numdata[is.na(numdata$unemp_r),5]
View(numdata)

#Geração de dados padronizados
scaledata <- as.matrix(scale(numdata))
View(scaledata)

#Visualização inicial
boxplot(numdata)

#grupo interno soma de méias
wss <- (nrow(numdata) - 1) * sum(apply(numdata, 2, var))

for (i in 2:15) wss[i] <- sum(kmeans(numdata, centers=i)$withinss)
?kmeans

plot(1:15, wss, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")

#Escolhendo 3 fatores para o k-means clustering
View(scaledata)
fit <- kmeans(scaledata, 3)
plot(scaledata, col=fit$cluster, pch=15)
?plot


#Gráfico dos pontos x 2 fatores ortogonais principais
clusplot(scaledata, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

