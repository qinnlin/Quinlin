
#### Compare Garch-estimated volatility with VIX(option/ )   (two different way to estimate sigma/risk)
#### Three columns in VIX.csv: date, closing price on SP500, VIX


rm(list=ls())                                  

vix <- read.csv('5-VIX.csv', header=TRUE)              ## http://www.cboe.com/products/vix-index-volatility/vix-options-and-futures/vix-index/vix-historical-data
vix$VIX  <- vix$VIX/1000  #vix the annualized  make it smaller to plot on one pic
Ret <- diff(log(vix$Close))
vix <- cbind(vix[-1,], Ret) #omit the last one
T   <- nrow(vix)



library('fGarch')
vol_garch <- numeric(T)

for (i in 500:T){
  retwindow   <- Ret[(i-500+1):i]

  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  sigmafit <- fit2@sigma.t
  vol_garch[i] <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigmafit[500]^2)
}

class(fit2)

plot(vol_garch[500:T], col='red', type='l', ylim=c(0,0.15), lwd=4)
points(vix$VIX[500:T], col='blue', type='l', lwd=4)

plot(vol_garch[500:T], vix$VIX[500:T])
cor(vol_garch[500:T], vix$VIX[500:T])



## vix QQplot

rm(list=ls())                                  

vix <- read.csv('5-VIX.csv', header=TRUE) 

plot(vix$VIX,type='l')
qqnorm(vix$VIX)
qqline(vix$VIX,col="red",lwd=4)
#thinner left tail and fatter right tail

qqnorm(-vix$VIX)   #take negative vix
qqline(-vix$VIX,col="red",lwd=4)
