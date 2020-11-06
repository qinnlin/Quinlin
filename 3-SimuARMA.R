



rm(list=ls())
dd <- arima.sim(model = list(ar =c(0.5,0.4)), n = 10000, sd = 0.1) #ar=phy1=0.9  or ar=0.5r(t-1)+0.4r(t-2)
par(mfrow=c(1,2))
acf(dd)
#acf(dd)$acf  
sd(dd)
pacf(dd)
#pacf(dd,5)$acf



rm(list=ls())
### Reproducing Figure 3.2
ts1 <- arima.sim( model = list(ar = 0.99), n = 100000, sd = 0.1)
autocorr1 <- acf(ts1, 100)$acf[1:101]
ts2 <- arima.sim( model = list(ar = 0.5),  n = 100000, sd = 0.1)
autocorr2 <- acf(ts2, 100)$acf[1:101]
ts3 <- arima.sim( model = list(ar = 0.1),  n = 100000, sd = 0.1)
autocorr3 <- acf(ts3, 100)$acf[1:101]
ts4 <- arima.sim( model = list(order=c(0, 1, 0)), n = 100000, sd = 0.1)#random walk model

autocorr4 <- acf(ts4, 100)$acf[1:101]

plot(autocorr1, col='red', type='l', ylim=c(0, 1.2), lwd=5)
points(autocorr2, col='green', type='l', lwd=5)
points(autocorr3, col='blue', type='l', lwd=5)
points(autocorr4, col='black', type='l', lwd=5)

