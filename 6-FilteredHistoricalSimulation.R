
#### This code illustrates a new method for estimating VAR: Filtered Historical Simulation


rm(list=ls())                                  ### Clean the R workspace

sp500 <- read.csv('1-sp500.csv', header=TRUE)       ### Load the daily return data of SP500 index
price <- sp500$Close                           ### Extract the price information

T     <- length(price)

ret   <- log(price[2:T]) - log(price[1:(T-1)]) ### Calculate the log return 

ind   <- which(ret!=0)                         ### Only keep those returns that are not zero (i.e. not on holidays)

ret   <- ret[ind]
T     <- length(ret)



#### Filtered Historical simulation
library('fGarch')
var1_hs  <- numeric(T)#historical simulation
var1_fhs  <- numeric(T)#future(filtered) historical simulation# correct
var1_garch <- numeric(T)#garch version paramatic version #assuming normal distribution


for (i in 501:T){
  retwindow   <- ret[(i-500):(i-1)]
  var1_hs[i] <- -quantile(retwindow, probs=0.01)
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  retstand <- retwindow/sigma
  var1_fhs[i]  <- -sigmapred*quantile(retstand, probs=0.01)
  
  var1_garch[i] <- -qnorm(0.01, mean=0, sd=sigmapred)
}

plot(var1_fhs, col='red', type='l', ylim=c(0,0.2), lwd=4)
points(var1_garch, col='blue', type='l', lwd=4) #assume normal distribution
##no tail at begining, but at finnacial crisis FHS CAN CAPTURE THE TAIL EVENT 





#### student t version of VaR
library('fGarch')
var1_t  <- numeric(T)  #var with student t #correct
var1_garch <- numeric(T) #garch
df_t    <- numeric(T) #degree freedom with student t 


for (i in 501:T){
  retwindow   <- ret[(i-500):(i-1)]

  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  var1_garch[i] <- -qnorm(0.01, mean=0, sd=sigmapred)
  
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE, cond.dist='std')  #conditional.dist='std' means using student t distribution
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  shape <- fit2@fit$coef[5]
  df_t[i] <- shape
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  var1_t[i] <- -qt(0.01, df=shape)/sqrt(shape/(shape-2))*sigmapred  #Student t Version of VaR
}
#qt means quantile of student t distribution with 1% porb and df
par(mfrow=c(2,1))
plot(var1_t, col='red', type='l', ylim=c(0,0.18), lwd=4)
points(var1_garch, col='blue', type='l', lwd=4)
plot(df_t, type='l')  #the lower degree freedom the fatter the tail










