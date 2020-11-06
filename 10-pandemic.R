

##Question1: compare volatility 
# 2008 / now / black monday(1987) / depression(1928)

### Download stock prices from Yahoo Finance
rm(list=ls())
library(quantmod)

# First take a look at VIX
getSymbols('^VIX',  from ="1985-12-31", to = "2020-12-31")   ## VIX index from CBOE
plot(VIX[, 6])
##vix is option market index starts from 1990, not enough for me.


# Now look at SP500
getSymbols('^GSPC', from ="1900-01-01", to = "2020-12-31")   ## ^GSPC is the yahoo finance symbol for SP500
ret <- diff(log(GSPC$GSPC.Adjusted))
ret <- ret[-1, ]      

## How does the covid19 pandamic episode compare with the history? 
library('fGarch')
fit2 <- garchFit( formula = ~garch(1, 1), data = ret, trace = FALSE)
ret$vol <- fit2@sigma.t
plot(ret$vol, type='l')








##Question 2: check the tail risk

# download data
getSymbols('^GSPC', from ="2000-01-01", to = "2020-12-31")   ## ^GSPC is the yahoo finance symbol for SP500
ret <- diff(log(GSPC$GSPC.Adjusted))
ret <- ret[-1, ]   

#### Filtered Historical simulation
library('fGarch')
N <- length(ret)
var1_garch <- numeric(N)  #garch version 
var1_fhs   <- numeric(N)
var1_t     <- numeric(N)  #student t
df_t       <- numeric(N)   #


for (i in 501:N){
  retwindow   <- ret[(i-500):(i-1)]
  
  #fitted historical version
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  omega <- fit2@fit$par[2]
  alpha <- fit2@fit$par[3]
  beta  <- fit2@fit$par[4]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*ret[500]^2 + beta*sigma[500]^2)
  retstand <- retwindow/sigma
  var1_fhs[i]  <- -sigmapred*quantile(retstand, probs=0.01)
  
  #garch version
  var1_garch[i] <- -qnorm(0.01, mean=0, sd=sigmapred)
  
  #student t version
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE, cond.dist='std')
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  shape <- fit2@fit$coef[5]
  df_t[i] <- shape #degree freedom for strudent t
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  var1_t[i] <- -qt(0.01, df=shape)/sqrt(shape/(shape-2))*sigmapred 
}

par(mfrow=c(3,1))
plot(var1_fhs, col='red', type='l', ylim=c(0,0.3), lwd=4)
points(var1_garch, col='blue', type='l', lwd=4)
#red line is higher than blue line, have tail risk in market 


plot(var1_t, col='red', type='l', ylim=c(0,0.3), lwd=4)
points(var1_garch, col='blue', type='l', lwd=4)
#red line (student t) is above significant amount than blue(garch) line
#indicating risk tail in those pandemic period

plot(df_t, type='l')
#degree of freedom is getting smaller, meaning less like normal distribution
#the lower the degree of freedom means the higher the amount tail risk












##Question 3: whether the market exhibit the tail risk as we expected?


dd1 <- .5*ret4 + .5*ret6
dd2 <- .5*ret4 + .5*ret13
plot(exp(cumsum(dd2)), type='l')
points(exp(cumsum(dd1)), type='l', col='red')


plot(exp(cumsum(ret4)), type='b')



points(cumsum(ret12), type='b', col='red')


tail(exp(cumsum(ret4))-1)*100
tail(exp(cumsum(ret6))-1)*100


