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

options(repr.plot.width = 8, repr.plot.height = 6)
qqnorm(adm$ret, main='QQ PLOT')
qqline(adm$ret)

## B. Realized volatility, positive realized semi-volatility, negative realized semi-volatility
summary(adm$RV)
summary(adm$RV_p)
summary(adm$RV_n)

options(repr.plot.width = 16, repr.plot.height = 6)
plot.zoo(adm$RV,xlab=NA,ylab=NA, main="RV")
lines(zoo(adm$RV_p),col="green", lwd=1)
lines(zoo(adm$RV_n),col="red", lwd=1)

legend("topleft", ncol = 1, legend = c('RV', 'positive RV', 'negative RV'),
       col = c('black', 'green', 'red'), lwd = 2, bty = 'n')

options(repr.plot.width = 16, repr.plot.height = 8)
par(mfrow = c(3, 2))
acf(adm$RV, main="Realized volatility")
pacf(adm$RV, main="")
acf(adm$RV_p, main="Realized volatility - positive")
pacf(adm$RV_p, main="")
acf(adm$RV_n, main="Realized volatility - negative")
pacf(adm$RV_n, main="")

## C. Realized skewness and realized kurtosis
summary(adm$RS)
summary(adm$RK)

options(repr.plot.width = 16, repr.plot.height = 8)
par(mfrow = c(2,1))
plot.zoo(adm$RS, main="Realised Skewness")
plot.zoo(adm$RK, main="Realised Kurtosis")

# 2. In-sample fit
adm$RV_lag <- shift(adm$RV)
adm$RV5_lag <- frollmean(adm$RV_lag, 5)
adm$RV22_lag <- frollmean(adm$RV_lag, 22)
adm$RV_p_lag <- shift(adm$RV_p)
adm$RV_n_lag <- shift(adm$RV_n)
adm$RS_lag <- shift(adm$RS)
adm$RK_lag <- shift(adm$RK)

## A. AR(1)-RV
AR1_RV <- lm(RV ~ RV_lag, data=adm, na.action=na.omit)
summary(AR1_RV)

options(repr.plot.width = 16, repr.plot.height = 6)
par(mfrow = c(1, 2))
acf(AR1_RV$residuals, main = 'ACF')
pacf(AR1_RV$residuals, main  = 'PACF')

## B. HAR
HAR <- lm(RV ~ RV_lag + RV5_lag + RV22_lag, data=adm)
summary(HAR)

par(mfrow = c(1, 2))
acf(HAR$residuals, main = 'ACF')
pacf(HAR$residuals, main = 'PACF')

## C. HAR-RS
HAR_RS <- lm(RV ~ RV_p_lag + RV_n_lag + RV5_lag + RV22_lag, data=adm)
summary(HAR_RS)

par(mfrow = c(1, 2))
acf(HAR_RS$residuals, main = 'ACF')
pacf(HAR_RS$residuals, main = 'PACF')

## D. HAR-RSkew-RKurt
HAR_RS_RK <- lm(RV ~ RV_lag + RV5_lag + RV22_lag + RS_lag + RK_lag, data=adm)
summary(HAR_RS_RK)

par(mfrow = c(1, 2))
acf(HAR_RS_RK$residuals, main = NA)
pacf(HAR_RS_RK$residuals, main = NA)

## E. ARMA-GARCH
(fit_auto <- auto.arima(adm$ret, ic = c("bic"), stationary = TRUE))
arima00 <- arima(adm$ret, order = c(0, 0, 0))
arch.test(arima00)

jarque.bera.test(arima00$residual)

# find the best GARCH model based on the BIC
p_max <- 5
q_max <- 5
aic_min <- Inf
best_p <- 0
best_q <- 0

for (i1 in 1:p_max) {
    for (i2 in 1:q_max) {
        garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                     variance.model = list(model = 'sGARCH', garchOrder = c(i1, i2)))
        garch_fit <- ugarchfit(spec = garchspec, data = adm$ret)
        inf_crit <- infocriteria(garch_fit)[2] #1 aic, 2 bic
        aic_min <- ifelse(inf_crit < aic_min, inf_crit, aic_min)
        
        best_p <- ifelse(inf_crit == aic_min, i1, best_p)
        best_q <- ifelse(inf_crit == aic_min, i2, best_q)
    }
}

c(best_p, best_q)

# specify the model that we want to estimate
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), 
                     variance.model = list(garchOrder = c(best_p, best_q)))
# estimate the model
garch_fit <- ugarchfit(spec = garchspec, data = adm$ret)
garch_fit

res_garch_fit <- residuals(garch_fit, standardize = TRUE)

par(mfrow=c(2,2))
acf(res_garch_fit, main = "Residuals")
pacf(res_garch_fit, main="")

Acf(res_garch_fit ** 2, main="Squared Residuals")
Pacf(res_garch_fit ** 2, main="")

## F. Realized GARCH
real_garchspec<- ugarchspec(variance.model = list(model = 'realGARCH', garchOrder = c(1, 1)),
    mean.model = list(armaOrder=c(0, 0), include.mean=FALSE))
real_garch_fit<- ugarchfit(real_garchspec, adm$ret, solver = 'gosolnp', realizedVol = sqrt(adm$RV))
real_garch_fit

res_real_garch_fit <- residuals(real_garch_fit, standardize = TRUE)

par(mfrow=c(2,2))
acf(res_real_garch_fit, main = "Residuals")
pacf(res_real_garch_fit, main="")

Acf(res_real_garch_fit ** 2, main="Squared Residuals")
Pacf(res_real_garch_fit ** 2, main="")

# Comparison of the models
AR1_RV_fit <- xts(fitted(AR1_RV), order.by=as.Date(names(fitted(AR1_RV))))
HAR_fit <- xts(fitted(HAR), order.by=as.Date(names(fitted(HAR))))
HAR_RS_fit <- xts(fitted(HAR_RS), order.by=as.Date(names(fitted(HAR_RS))))
HAR_RS_RK_fit <- xts(fitted(HAR_RS_RK), order.by=as.Date(names(fitted(HAR_RS_RK))))

options(repr.plot.width = 20, repr.plot.height = 10)

par(mfrow = c(2, 2))

plot.zoo(adm$RV, col="black", xlab=NA,ylab=NA, main="RV vs AR(1)-RV")
lines(zoo(AR1_RV_fit), col="red")

plot.zoo(adm$RV, col="black", xlab=NA,ylab=NA, main="RV vs HAR")
lines(zoo(HAR_fit), col="red")

plot.zoo(adm$RV, col="black", xlab=NA,ylab=NA, main="RV vs HAR_RS")
lines(zoo(HAR_RS_fit), col="red")

plot.zoo(adm$RV, col="black", xlab=NA,ylab=NA, main="RV vs HAR_RS_RK")
lines(zoo(HAR_RS_RK_fit), col="red")

options(repr.plot.width = 20, repr.plot.height = 10)
plot.zoo(adm$RV,xlab=NA,ylab=NA, main = "RV vs GARCH vs Realized GARCH")
lines(zoo(sigma(garch_fit)),col="red", lwd = 2)
lines(zoo(sigma(real_garch_fit)),col="blue", lwd = 2)
legend("topright", ncol = 1, legend = c('RV', 'GARCH', 'Realized GARCH'), col = c('black', 'red', 'blue'), lwd = 2, bty = 'n')

# 3. Forecast
w_l <- 750
n_for <- 750
RV_true <- adm$RV[751:1500]

start <- 1
end <- 750
AR1_exp <- rep(NA, 750)
AR1_roll <- rep(NA, 750)
HAR_exp <- rep(NA, 750)
HAR_roll <- rep(NA, 750)
HAR_RS_exp <- rep(NA, 750)
HAR_RS_roll <- rep(NA, 750)
HAR_RS_RK_exp <- rep(NA, 750)
HAR_RS_RK_roll <- rep(NA, 750)

# rolling
for (i in 0:(750-1)) {
    training <- adm[(start + i):(end + i), ]                 
    testing <- adm[end + i + 1,]
    
    AR1_RV <- lm(RV ~ RV_lag, data=training)
    AR1_roll[i + 1] <- predict(AR1_RV, newdata=testing)
    
    HAR <- lm(RV ~ RV_lag + RV5_lag + RV22_lag, data=training)
    HAR_roll[i + 1] <- predict(HAR, newdata=testing)
    
    HAR_RS <- lm(RV ~ RV_p_lag + RV_n_lag + RV5_lag + RV22_lag, data=training)
    HAR_RS_roll[i + 1] <- predict(HAR_RS, newdata=testing)
    
    HAR_RS_RK <- lm(RV ~ RV_lag + RV5_lag + RV22_lag + RS_lag + RK_lag, data=training)
    HAR_RS_RK_roll[i + 1] <- predict(HAR_RS_RK, newdata=testing)
}

# expanding
for (i in 0:(750-1)) {
    training <- adm[start:(end + i), ]
    testing <- adm[end + i + 1,]
    
    AR1_RV <- lm(RV ~ RV_lag, data=training)
    AR1_exp[i + 1] <- predict(AR1_RV, newdata=testing)
    
    HAR <- lm(RV ~ RV_lag + RV5_lag + RV22_lag, data=training)
    HAR_exp[i + 1] <- predict(HAR, newdata=testing)
    
    HAR_RS <- lm(RV ~ RV_p_lag + RV_n_lag + RV5_lag + RV22_lag, data=training)
    HAR_RS_exp[i + 1] <- predict(HAR_RS, newdata=testing)
    
    HAR_RS_RK <- lm(RV ~ RV_lag + RV5_lag + RV22_lag + RS_lag + RK_lag, data=training)
    HAR_RS_RK_exp[i + 1] <- predict(HAR_RS_RK, newdata=testing)
}

GARCH_roll_exp<-list()

system.time(
GARCH_roll_exp[[1]]<-ugarchroll(garchspec, adm$ret, n.ahead = 1, forecast.length = n_for, 
                                 n.start = NULL, refit.every = 1, refit.window = c("moving"), 
                                 window.size = w_l, solver = "solnp", fit.control = list(), 
                                 solver.control = list(), calculate.VaR = FALSE, 
                                 cluster = NULL, keep.coef = TRUE)
)

system.time(
GARCH_roll_exp[[2]]<-ugarchroll(garchspec, adm$ret, n.ahead = 1, forecast.length = n_for, 
                                n.start = NULL, refit.every = 1, refit.window = c("recursive"), 
                                window.size = w_l, solver = "solnp", fit.control = list(), 
                                solver.control = list(), calculate.VaR = FALSE, 
                                cluster = NULL, keep.coef = TRUE)
)

sigma_GARCH_roll_for <- GARCH_roll_exp[[1]]@forecast$density$Sigma
sigma_GARCH_exp_for <- GARCH_roll_exp[[2]]@forecast$density$Sigma

system.time(
real_GARCH_roll<-ugarchroll(real_garchspec, adm$ret, n.ahead = 1, forecast.length = n_for, 
                            n.start = NULL, refit.every = 1, refit.window = c("moving"), 
                            window.size = w_l, solver = "gosolnp", fit.control = list(), 
                            solver.control = list(), calculate.VaR = FALSE, 
                            cluster = NULL, keep.coef = FALSE,realizedVol = sqrt(adm$RV))
)

real_GARCH_roll_1 <- resume(real_GARCH_roll, solver = "gosolnp")

real_GARCH_roll_2 <- resume(real_GARCH_roll_1, solver = "gosolnp")
sigma_real_GARCH_roll_for <- real_GARCH_roll_2@forecast[["density"]]$Sigma
system.time(
real_GARCH_exp<-ugarchroll(real_garchspec, adm$ret, n.ahead = 1, forecast.length = n_for, 
                            n.start = NULL, refit.every = 1, refit.window = c("recursive"), 
                            window.size = w_l, solver = "gosolnp", fit.control = list(), 
                            solver.control = list(), calculate.VaR = FALSE, 
                            cluster = NULL, keep.coef = FALSE,realizedVol = sqrt(adm$RV))
)

real_GARCH_exp_1 <- resume(real_GARCH_exp, solver = "gosolnp")

real_GARCH_exp_2 <- resume(real_GARCH_exp_1, solver = "gosolnp")
sigma_real_GARCH_exp_for <- real_GARCH_exp_2@forecast[["density"]]$Sigma

# Plots
options(repr.plot.width = 20, repr.plot.height = 10)
par(mfrow = c(2, 2))

plot.zoo(RV_true, lwd=1.5, col="black", ylim=c(0.002,0.04), main="RV vs AR(1)")
lines(zoo(AR1_roll, order.by=index(RV_true)), col="red")
lines(zoo(AR1_exp, order.by=index(RV_true)), col="blue")
legend("topleft", ncol = 1, legend = c('RV', 'AR1 rolling', 'AR1 expanding'), col = c('black', 'red', 'blue'), lwd = 2, bty = 'n')

plot.zoo(RV_true, lwd=1.5, col="black", ylim=c(0.002,0.04), main="RV vs HAR")
lines(zoo(HAR_roll, order.by=index(RV_true)), col="red")
lines(zoo(HAR_exp, order.by=index(RV_true)), col="blue")
legend("topleft", ncol = 1, legend = c('RV', 'HAR rolling', 'HAR expanding'), col = c('black', 'red', 'blue'), lwd = 2, bty = 'n')

plot.zoo(RV_true, lwd=1.5, col="black", ylim=c(0.002,0.04), main="RV vs HAR_RS")
lines(zoo(HAR_RS_roll, order.by=index(RV_true)), col="red")
lines(zoo(HAR_RS_exp, order.by=index(RV_true)), col="blue")
legend("topleft", ncol = 1, legend = c('RV', 'HAR_RS rolling', 'HAR_RS expanding'), col = c('black', 'red', 'blue'), lwd = 2, bty = 'n')

plot.zoo(RV_true, lwd=1.5, col="black", ylim=c(0.002,0.04), main="RV vs HAR_RS_RK")
lines(zoo(HAR_RS_RK_roll, order.by=index(RV_true)), col="red")
lines(zoo(HAR_RS_RK_exp, order.by=index(RV_true)), col="blue")
legend("topleft", ncol = 1, legend = c('RV', 'HAR_RS_RK rolling', 'HAR_RS_RK expanding'), col = c('black', 'red', 'blue'), lwd = 2, bty = 'n')

plot.zoo(RV_true,xlab=NA,ylab=NA, main = "RV vs GARCH rolling vs GARCH expanding")
lines(zoo(sigma_GARCH_roll_for, order.by=index(RV_true)),col="red", lwd = 2)
lines(zoo(sigma_GARCH_exp_for, order.by=index(RV_true)),col="blue", lwd = 2)
legend("topleft", ncol = 1, legend = c('RV', 'GARCH rolling', 'GARCH expanding'), col = c('black', 'red', 'blue'), lwd = 2, bty = 'n')

plot.zoo(RV_true,xlab=NA,ylab=NA, main = "RV vs Realized GARCH rolling vs Realized GARCH expanding")
lines(zoo(sigma_real_GARCH_roll_for, order.by=index(RV_true)),col="red",lwd=2)
lines(zoo(sigma_real_GARCH_exp_for, order.by=index(RV_true)),col="blue",lwd=2)
legend("topright", ncol = 1, legend = c('RV', 'Realized GARCH rolling', 'Realized GARCH expanding'), col = c('black', 'red', 'blue'), lwd = 2, bty = 'n')

boxplot.stats(sigma_real_GARCH_roll_for)

boxplot.stats(sigma_real_GARCH_exp_for)

sigma_real_GARCH_roll_for_clear <- sigma_real_GARCH_roll_for[sigma_real_GARCH_roll_for %in% boxplot.stats(sigma_real_GARCH_roll_for)$out] <- NA
sigma_real_GARCH_exp_for_clear <- sigma_real_GARCH_exp_for[sigma_real_GARCH_exp_for %in% boxplot.stats(sigma_real_GARCH_exp_for)$out] <- NA
plot.zoo(RV_true, col="black", main="Realized GARCH")
lines(zoo(sigma_real_GARCH_roll_for, order.by=index(RV_true)), col="red", lwd=2)
lines(zoo(sigma_real_GARCH_exp_for, order.by=index(RV_true)), col="blue", lwd=2)
legend("topleft", ncol = 1, legend = c('RV', 'Realized GARCH rolling', 'Realized GARCH expanding'), col = c('black', 'red', 'blue'), lwd = 2, bty = 'n')

#Forecast errors
AR1_roll_e <- RV_true - AR1_roll
AR1_exp_e <- RV_true - AR1_exp

HAR_roll_e <- RV_true - HAR_roll
HAR_exp_e <- RV_true - HAR_exp

HAR_RS_roll_e <- RV_true - HAR_RS_roll
HAR_RS_exp_e <- RV_true - HAR_RS_exp

HAR_RS_RK_roll_e <- RV_true - HAR_RS_RK_roll
HAR_RS_RK_exp_e <- RV_true - HAR_RS_RK_exp

sigma_GARCH_roll_for_e <- RV_true - sigma_GARCH_roll_for
sigma_GARCH_exp_for_e <- RV_true - sigma_GARCH_exp_for

sigma_real_GARCH_roll_for_e <- RV_true - sigma_real_GARCH_roll_for
sigma_real_GARCH_exp_for_e <- RV_true - sigma_real_GARCH_exp_for

options(repr.plot.width = 20, repr.plot.height = 10)
par(mfrow = c(3, 2))

plot.zoo(AR1_roll_e, ylim=c(-0.01, 0.03), main="AR(1) - rolling")
plot.zoo(AR1_exp_e, ylim=c(-0.01, 0.03), main="AR(1) - expanding")

plot.zoo(HAR_roll_e, ylim=c(-0.01, 0.03), main="HAR - rolling")
plot.zoo(HAR_exp_e, ylim=c(-0.01, 0.03), main="HAR - expanding")

plot.zoo(HAR_RS_roll_e, ylim=c(-0.01, 0.03), main="HAR-RS - rolling")
plot.zoo(HAR_RS_exp_e, ylim=c(-0.01, 0.03), main="HAR-RS - expanding")

plot.zoo(HAR_RS_RK_roll_e, ylim=c(-0.01, 0.03), main="HAR_RS_RK - rolling")
plot.zoo(HAR_RS_RK_exp_e, ylim=c(-0.01, 0.03), main="HAR_RS_RK - expanding")

plot.zoo(sigma_GARCH_roll_for_e, ylim=c(-0.01, 0.03), main="ARMA - GARCH - rolling")
plot.zoo(sigma_GARCH_exp_for_e, ylim=c(-0.01, 0.03), main="ARMA - GARCH - expanding")

plot.zoo(sigma_real_GARCH_roll_for_e, ylim=c(-0.01, 0.03), main="Realized GARCH - rolling")
plot.zoo(sigma_real_GARCH_exp_for_e, ylim=c(-0.01, 0.03), main="Realized GARCH - expanding")

mean_errors <- matrix(ncol = 4, nrow = 6)
colnames(mean_errors) <- c('MAE - rolling', 'MAE - expanding', "MSE - rolling","MSE - expanding")
rownames(mean_errors) <- c('AR(1)', 'HAR', 'HAR-RS', "HAR_RS_RK", "ARMA - GARCH", "Realized GARCH" )

mean_errors[1, 1] <- mean(abs(AR1_roll_e), na.rm=TRUE)
mean_errors[1, 2] <- mean(abs(AR1_exp_e), na.rm=TRUE)
mean_errors[1, 3] <- mean(AR1_roll_e^2, na.rm=TRUE)
mean_errors[1, 4] <- mean(AR1_exp_e^2, na.rm=TRUE)

mean_errors[2, 1] <- mean(abs(HAR_roll_e), na.rm=TRUE)
mean_errors[2, 2] <- mean(abs(HAR_exp_e), na.rm=TRUE)
mean_errors[2, 3] <- mean(HAR_roll_e^2, na.rm=TRUE)
mean_errors[2, 4] <- mean(HAR_exp_e^2, na.rm=TRUE)

mean_errors[3, 1] <- mean(abs(HAR_RS_roll_e), na.rm=TRUE)
mean_errors[3, 2] <- mean(abs(HAR_RS_exp_e), na.rm=TRUE)
mean_errors[3, 3] <- mean(HAR_RS_roll_e^2, na.rm=TRUE)
mean_errors[3, 4] <- mean(HAR_RS_exp_e^2, na.rm=TRUE)

mean_errors[4, 1] <- mean(abs(HAR_RS_RK_roll_e), na.rm=TRUE)
mean_errors[4, 2] <- mean(abs(HAR_RS_RK_exp_e), na.rm=TRUE)
mean_errors[4, 3] <- mean(HAR_RS_RK_roll_e^2, na.rm=TRUE)
mean_errors[4, 4] <- mean(HAR_RS_RK_exp_e^2, na.rm=TRUE)

mean_errors[5, 1] <- mean(abs(sigma_GARCH_roll_for_e), na.rm=TRUE)
mean_errors[5, 2] <- mean(abs(sigma_GARCH_exp_for_e), na.rm=TRUE)
mean_errors[5, 3] <- mean(sigma_GARCH_roll_for_e^2, na.rm=TRUE)
mean_errors[5, 4] <- mean(sigma_GARCH_exp_for_e^2, na.rm=TRUE)

mean_errors[6, 1] <- mean(abs(sigma_real_GARCH_roll_for_e), na.rm=TRUE)
mean_errors[6, 2] <- mean(abs(sigma_real_GARCH_exp_for_e), na.rm=TRUE)
mean_errors[6, 3] <- mean(sigma_real_GARCH_roll_for_e^2, na.rm=TRUE)
mean_errors[6, 4] <- mean(sigma_real_GARCH_exp_for_e^2, na.rm=TRUE)

mean_errors

dm.test(AR1_roll_e, AR1_exp_e , alternative="two.sided", power=2)
dm.test(HAR_roll_e, HAR_exp_e , alternative="greater", power=2)
dm.test(HAR_RS_roll_e, HAR_RS_exp_e , alternative="greater", power=2)
dm.test(HAR_RS_RK_roll_e, HAR_RS_RK_exp_e , alternative="two.sided", power=2)
dm.test(sigma_GARCH_roll_for_e, sigma_GARCH_exp_for_e , alternative="less", power=2)
dm.test(na.omit(sigma_real_GARCH_roll_for_e), na.omit(sigma_real_GARCH_exp_for_e)

dm.test(AR1_exp_e, HAR_RS_RK_exp_e , alternative="greater", power=2)
dm.test(sigma_GARCH_roll_for_e, HAR_RS_RK_exp_e , alternative="greater", power=2)
dm.test(na.omit(sigma_real_GARCH_exp_for_e) , HAR_RS_RK_exp_e , alternative="greater", power=2)

dm.test(HAR_exp_e, HAR_RS_exp_e , alternative="two.sided", power=2)
dm.test(HAR_exp_e, HAR_RS_RK_exp_e , alternative="two.sided", power=2)
dm.test(HAR_RS_exp_e, HAR_RS_RK_exp_e , alternative="two.sided", power=2)

dm.test(HAR_exp_e, HAR_RS_exp_e , alternative="greater", power=2)
dm.test(HAR_exp_e, HAR_RS_RK_exp_e , alternative="greater", power=2)
dm.test(HAR_RS_exp_e, HAR_RS_RK_exp_e , alternative="greater", power=2)

MZ_AR1_roll<-lm(RV_true~AR1_roll)
summary(MZ_AR1_roll)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_AR1_roll_coef<-summary(MZ_AR1_roll)$coefficients
(MZ_AR1_roll_coef[2,"Estimate"]-1)/MZ_AR1_roll_coef[2,"Std. Error"]

MZ_AR1_exp<-lm(RV_true~AR1_exp)
summary(MZ_AR1_exp)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_AR1_exp_coef<-summary(MZ_AR1_exp)$coefficients
(MZ_AR1_exp_coef[2,"Estimate"]-1)/MZ_AR1_exp_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_HAR_roll<-lm(RV_true~HAR_roll)
summary(MZ_HAR_roll)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_HAR_roll_coef<-summary(MZ_HAR_roll)$coefficients
(MZ_HAR_roll_coef[2,"Estimate"]-1)/MZ_HAR_roll_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_HAR_exp<-lm(RV_true~HAR_exp)
summary(MZ_HAR_exp)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_HAR_exp_coef<-summary(MZ_HAR_exp)$coefficients
(MZ_HAR_exp_coef[2,"Estimate"]-1)/MZ_HAR_exp_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_HAR_RS_roll<-lm(RV_true~HAR_RS_roll)
summary(MZ_HAR_RS_roll)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_HAR_RS_roll_coef<-summary(MZ_HAR_RS_roll)$coefficients
(MZ_HAR_RS_roll_coef[2,"Estimate"]-1)/MZ_HAR_RS_roll_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_HAR_RS_exp<-lm(RV_true~HAR_RS_exp)
summary(MZ_HAR_RS_exp)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_HAR_RS_exp_coef<-summary(MZ_HAR_RS_exp)$coefficients
(MZ_HAR_RS_exp_coef[2,"Estimate"]-1)/MZ_HAR_RS_exp_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_HAR_RS_RK_roll<-lm(RV_true~HAR_RS_RK_roll)
summary(MZ_HAR_RS_RK_roll)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_HAR_RS_RK_roll_coef<-summary(MZ_HAR_RS_RK_roll)$coefficients
(MZ_HAR_RS_RK_roll_coef[2,"Estimate"]-1)/MZ_HAR_RS_RK_roll_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_HAR_RS_RK_exp<-lm(RV_true~HAR_RS_RK_exp)
summary(MZ_HAR_RS_RK_exp)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_HAR_RS_RK_exp_coef<-summary(MZ_HAR_RS_RK_exp)$coefficients
(MZ_HAR_RS_RK_exp_coef[2,"Estimate"]-1)/MZ_HAR_RS_RK_exp_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_sigma_GARCH_roll_for<-lm(RV_true~sigma_GARCH_roll_for)
summary(MZ_sigma_GARCH_roll_for)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_sigma_GARCH_roll_for_coef<-summary(MZ_sigma_GARCH_roll_for)$coefficients
(MZ_sigma_GARCH_roll_for_coef[2,"Estimate"]-1)/MZ_sigma_GARCH_roll_for_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_sigma_GARCH_exp_for<-lm(RV_true~sigma_GARCH_exp_for)
summary(MZ_sigma_GARCH_exp_for)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_sigma_GARCH_exp_for_coef<-summary(MZ_sigma_GARCH_exp_for)$coefficients
(MZ_sigma_GARCH_exp_for_coef[2,"Estimate"]-1)/MZ_sigma_GARCH_exp_for_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_sigma_real_GARCH_roll_for<-lm(RV_true~sigma_real_GARCH_roll_for)
summary(MZ_sigma_real_GARCH_roll_for)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_sigma_real_GARCH_roll_for_coef<-summary(MZ_sigma_real_GARCH_roll_for)$coefficients
(MZ_sigma_real_GARCH_roll_for_coef[2,"Estimate"]-1)/MZ_sigma_real_GARCH_roll_for_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.

MZ_sigma_real_GARCH_exp_for<-lm(RV_true~sigma_real_GARCH_exp_for)
summary(MZ_sigma_real_GARCH_exp_for)

# The estimated coefficient is different from zero, but we want to test whether it is statistically different from 1. Therefore, we do following test.
MZ_sigma_real_GARCH_exp_for_coef<-summary(MZ_sigma_real_GARCH_exp_for)$coefficients
(MZ_sigma_real_GARCH_exp_for_coef[2,"Estimate"]-1)/MZ_sigma_real_GARCH_exp_for_coef[2,"Std. Error"]
# If the score is below -1.96 we can reject the null hypothesis that the coeffcient is 1.
