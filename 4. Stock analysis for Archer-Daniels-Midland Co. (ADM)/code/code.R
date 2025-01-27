#packages
install.packages('tseries')
install.packages('data.table')
install.packages('aod')
install.packages('zoo')
install.packages('xts')
install.packages('forecast')
install.packages('aTSA')
install.packages('car')
install.packages('rugarch')

library('tseries')
library('data.table')
library("aod")
library('zoo')
library('xts')
library('forecast')
library('aTSA')
library("car")
library('rugarch')

# 1. Data description
data <- load('11.RData')
summary(adm) #quick glance what to expect

## A. Returns
summary(adm$ret)

options(repr.plot.width = 16, repr.plot.height = 6)
plot.zoo(adm$ret,xlab=NA,ylab=NA, main="Returns")

options(repr.plot.width = 16, repr.plot.height = 5)
par(mfrow = c(1, 2))
acf(adm$ret, main="ACF")
pacf(adm$ret, main="PACF")

tseries::adf.test(adm$ret)
options(repr.plot.width = 8, repr.plot.height = 10)
hist(adm$ret, 35, probability=T, ylim=c(0,40),main="`Distribution of returns")
x <- seq(-0.10, 0.10, by = .001)
lines(x, dnorm(x, mean(adm$ret), sd(adm$ret)), col="red")
