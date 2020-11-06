

### Student t-distribution
rm(list=ls())
y <- rt(50000, df = 5)  #r:radom simulation rt:radom simulate student t distribution for 50000times, and degree of fredom  df=5
hist(y,500) #500 density
qqnorm(y) 
qqline(y,col='red',lwd=4)
sd(y)

#when  y <- rt(50000, df = 50)  df to 50, close to normal distrubution

hist(y, 500, col='red')

ystand <- y/sd(y)    ## standardized t distribution
sd(ystand)





### Maximumum likelihood estimation on SP500
rm(list=ls())
library('fGarch')
sp500 <- read.csv('1-SP500.csv', head=TRUE)
ret   <- diff(log(sp500$Close))
fit2 <- garchFit( formula = ~garch(1, 1), data = ret, trace = FALSE, cond.dist='std')
fit2






#### Filtered Historical simulation

var1_t  <- numeric(T)
var1_garch <- numeric(T)


for (i in 501:T){
  retwindow   <- ret[(i-500):(i-1)]
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  var1_garch[i] <- -qnorm(0.01, mean=0, sd=sigmapred)
  
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE, cond.dist='std')
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  shape <- fit2@fit$coef[5]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  var1_t[i] <- -qt(0.01, df=shape)/sqrt(shape/(shape-2))*sigmapred  
}

plot(var1_t, col='red', type='l', ylim=c(0,0.18), lwd=4)
points(var1_garch, col='blue', type='l', lwd=4)
