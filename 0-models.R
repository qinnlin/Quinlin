


###lecture 3 simuARIMA and stationary test
#1:AR
rm(list=ls())
dd <- arima.sim(model = list(ar =c(0.5,0.4)), n = 10000, sd = 0.1) #ar=phy1=0.9  or ar=0.5r(t-1)+0.4r(t-2)
par(mfrow=c(1,2))
acf(dd,10) #10 number of lag 
sd(dd)
pacf(dd)
#pacf(dd,5)$acf

# 2:MA 
dd <- arima.sim(model = list(ma = 1), n = 1000, sd = 0.1) 

#3:ARMA
dd <- arima.sim(model = list(ar = 0.9, ma = 0.9), n = 1000, sd = 0.1) #n number of simulation

#4:random walk
dd <- arima.sim( model = list(order=c(0, 1, 0)), n = 100000, sd = 0.1)   
#model = list(order=c(0, 1, 0)) ar and ma trun off, only random walk turn on 

#5:ARIMA
dd <- arima.sim( model = list(ar = 0.9, order=c(1, 1, 0)), n = 100000, sd = 0.1)
#order=c(1, 1, 0) ar=on radom=on ma=off ;ar(1)=0.9

#6:stationary test
library('tseries')
adf.test(ret, alternative='stationary')        ### When the p-value is larger than 5%, the series is considered non-stationary



# lecture 5: GARCH
# 1:Diagnostic check of GARCH models: Autocorrelation of standardized returns
rm(list=ls())
library('fGarch')
sp500 <- read.csv('1-SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))

fit2 <- garchFit( formula = ~garch(1, 1), data = Ret, trace = FALSE)
sigma <- fit2@sigma.t

Retstand <- Ret/sigma
par(mfrow=c(2,2))
acf(Ret,20)
acf(Retstand,20)
acf(Ret^2, 20)
acf(Retstand^2, 20)  ##strong test hat_retrun^2  can see the change


#2 VIx garch model 500 windows
rm(list=ls())                                  

vix <- read.csv('5-VIX.csv', header=TRUE)             
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
  vol_garch[i] <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigmafit[500]^2) #predicted sigma t+1
}

class(fit2)

plot(vol_garch[500:T], col='red', type='l', ylim=c(0,0.15), lwd=4)
points(vix$VIX[500:T], col='blue', type='l', lwd=4)

plot(vol_garch[500:T], vix$VIX[500:T])
cor(vol_garch[500:T], vix$VIX[500:T])



#lecture 6:  HS.FHS.GATCH.STUDENT T version of VAR; QQ plot for nonnormal

#1: filtered histotical simulation(FHS):a new method for estimating VAR
#calculate VAR: hs; fhs; garch; student t
rm(list=ls())                                  ### Clean the R workspace

sp500 <- read.csv('1-sp500.csv', header=TRUE)       ### Load the daily return data of SP500 index
ret  <- diff(log(sp500$Close )) 
T            <- length(ret)

#### Filtered Historical simulation
library('fGarch')
var1_hs  <- numeric(T)#historical simulation
var1_fhs  <- numeric(T)#future(filtered) historical simulation# correct
var1_garch <- numeric(T)#garch version paramatic version #assuming normal distribution
var1_t  <- numeric(T)  #var with student t #correct
df_t    <- numeric(T) #degree freedom with student t 


for (i in 501:T){
  retwindow   <- ret[(i-500):(i-1)]
  
  var1_hs[i] <- -quantile(retwindow, probs=0.01) #historical simulation
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  omega <- fit2@fit$coef[2]
  alpha <- fit2@fit$coef[3]
  beta  <- fit2@fit$coef[4]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  retstand <- retwindow/sigma
  var1_fhs[i]  <- -sigmapred*quantile(retstand, probs=0.01) #filtered histoical simulation
  
  var1_garch[i] <- -qnorm(0.01, mean=0, sd=sigmapred) #garch version
  
  fit3 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE, cond.dist='std')  #conditional.dist='std' means using student t distribution
  omega <- fit3@fit$coef[2]
  alpha <- fit3@fit$coef[3]
  beta  <- fit3@fit$coef[4]
  shape <- fit3@fit$coef[5]
  df_t[i] <- shape #degree of freedom
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*retwindow[500]^2 + beta*sigma[500]^2)
  var1_t[i] <- -qt(0.01, df=shape)/sqrt(shape/(shape-2))*sigmapred  #Student t Version of VaR
  #qt means quantile of student t distribution with 1% porb and df
}

plot(var1_fhs, col='red', type='l', ylim=c(0,0.2), lwd=4)
points(var1_garch, col='blue', type='l', lwd=4) #assume normal distribution
##no tail at begining, but at finnacial crisis FHS CAN CAPTURE THE TAIL EVENT 

par(mfrow=c(2,1))
plot(var1_t, col='red', type='l', ylim=c(0,0.18), lwd=4)
points(var1_garch, col='blue', type='l', lwd=4)
plot(df_t, type='l')  #the lower degree freedom the fatter the tail


##2: QQ-plots to reveal non-normality
qqnorm(Ret, main='S&P500 Returns')            ### Unconditional non-normality  #main is title    
qqline(Ret, col='red',lwd=4)            #lwd is wide   ### straight line correspond to normal distribution
##in the middle [-1,1] it follows normal distribution,but two tails are not

qqnorm(Retstand, main='Standardized S&P500 Returns')   ### Conditional non-normality   
qqline(Retstand, col='red',lwd=4)
# retstand diviation is smaller than ret


##lecture 7: Modeling covariance(simple,riskmatrix,garch) and correlation(garch)
rm(list=ls())
library('fGarch')
retdata <- read.csv('7-SP500Tnote.csv', head=TRUE)
retsp500   <- diff(log(retdata$sp500))
rettnote   <- diff(log(retdata$tnote)) 
T            <- length(retsp500)

plot(retsp500, type='l', lwd=2, col='blue')
points(rettnote, type='l', lwd=2, col='red')

# 1:simple covariance model: Rolling window 
windowlength <- 25
rollingCov   <- numeric(T)

for (i in 25:T){
  rollingCov[i] <- sum(retsp500[(i-24):i]*rettnote[(i-24):i])/25
}

plot(rollingCov, type='l', col='blue', lwd=4) #show covariance
##why 2008 have negative cov: sell stock and buy bond,so stock price down while bond price up; negative movement.


#2: Risk Metrics Covariance model: Exponentially smoothed covariance #doesnot matter about the first number
lambda      <- 0.94

exponentialCov  <- numeric(T)
for (i in 2:T){
  exponentialCov[i] <- (1-lambda)*retsp500[i-1]*rettnote[i-1] + lambda*exponentialCov[i-1]
}

plot(exponentialCov, type='l', col='blue', lwd=2)
points(rollingCov, type='l', col='red', lwd=2)

#3: Garch Covariance model
omega   <- 1e-6
alpha   <- 0.05
beta    <- 0.9
GarchCov  <- numeric(T)
for (i in 2:T){
  GarchCov[i] <- omega + alpha*retsp500[i-1]*rettnote[i-1] + beta*GarchCov[i-1]
}

plot(GarchCov, type='l', col='blue', lwd=2)
points(rollingCov, type='l', col='red', lwd=2)


#4: GARCH correlation model: Dynamic conditional correlation 
library('fGarch')
fit1  <- garchFit( formula = ~garch(1, 1), data = retsp500, trace = FALSE)
sigma1 <- fit1@sigma.t
retsp500stand <- retsp500/sigma1

fit2  <- garchFit( formula = ~garch(1, 1), data = rettnote, trace = FALSE)
sigma2 <- fit2@sigma.t
rettnotestand <- rettnote/sigma2


alpha       <- 0.05 #given
beta        <- 0.9
q11         <- numeric(T)
q12         <- numeric(T)
q22         <- numeric(T)
q11lr       <- mean(retsp500stand^2)
q12lr       <- mean(retsp500stand*rettnotestand)
q22lr       <- mean(rettnotestand^2)

for (i in 2:T){
  q11[i] <- q11lr + alpha*(retsp500stand[i-1]^2 - q11lr) + beta*(q11[i-1]-q11lr)
  q12[i] <- q12lr + alpha*(retsp500stand[i-1]*rettnotestand[i-1] - q12lr) + beta*(q12[i-1]-q12lr)
  q22[i] <- q22lr + alpha*(rettnotestand[i-1]^2 - q22lr) + beta*(q22[i-1]-q22lr)
}

GarchCorr <- q12/sqrt(q11*q22) #garch correlation
plot(q12,type='l',col='red') #beyond -1 not like covariance
plot(GarchCorr, type='l', col='blue', lwd=4)

##Lecture 8: term structure of risk: Monte Carlo
# Monte carlo: Rnorm & FHS(only step 2 are different)
rm(list=ls())
library('fGarch')
sp500 <- read.csv('1-SP500.csv', head=TRUE)
Ret   <- diff(log(sp500$Close))
result <- garchFit( formula = ~garch(1, 1), data = Ret[1:500], trace = FALSE)
omega <- result@fit$par[2]
alpha <- result@fit$par[3]
beta  <- result@fit$par[4]
sigma <- result@sigma.t
sigmapred <- sqrt(omega + alpha*Ret[500]^2 + beta*sigma[500]^2) ##sigma t+1

sigmapred/sqrt(omega/(1-alpha-beta))## compare with longrun sigma

sigmapred <- sqrt(omega/(1-alpha-beta))*2 ##if the begining sigma is 2 times bigger than long run sigma  downward sloping 
##change to *0.5   upward sloping

##step 2
MC <- 10000
T  <- 500
shock <- matrix(0, MC, T) #create a shock(Z) matrix, all zero
for (i in 1:T){
  shock[, i] <- rnorm(MC, 0, 1)
}
##colume by colume, because column only 500 times, row by row need 10000times, save time.
###################################
#if monte carlo fhs only different if step 2.
#MC <- 10000
#T  <- 500
#shock <- matrix(0, MC, T)
#for (i in 1:T){
#  shock[, i] <- sample(Retstand, MC, replace=T) 
#}


ReturnMC <- matrix(NA, MC, T) #return matrix
for (i in 1:MC){
  sigmapredMC <- sigmapred
  for (j in 1:T){
    ReturnMC[i, j] <- sigmapredMC*shock[i, j]
    sigmapredMC <- sqrt(omega + alpha*ReturnMC[i, j]^2 + beta*sigmapredMC^2)
  }
}

##step 3
ReturnMCT <- matrix(NA, MC, T) ##cumulated returns
for (i in 1:MC){
  ReturnMCT[i, ] <- cumsum(ReturnMC[i, ]) ##cumulative sum
}



###step 4 Value at Risk
VaRMC     <- numeric(T)
for (i in 1:T){
  VaRMC[i] <- -quantile(ReturnMCT[,i], probs=0.05)
}
par(mfrow=c(1,2))
plot(VaRMC, type='l', lwd=4)
plot(VaRMC/sqrt(1:T)/VaRMC[1], type='l', lwd=4)



###step 4-2: Expected Shortfall
ESMC     <- numeric(T)
for (i in 1:T){
  indES    <- which(ReturnMCT[,i] <= -VaRMC[i]) ##which tells the location
  ESMC[i]  <- -mean(ReturnMCT[indES,i])
}
par(mfrow=c(1,2))
plot(ESMC, type='l', lwd=4)
plot(ESMC/sqrt(1:T)/ESMC[1], type='l', lwd=4)

##lecture 9: Backtesting(2 test)
#step 1 same; step 2: unconditional coverage test; step 2-2: independent test

#step 1: data preparing and FHS
rm(list=ls())                                  

sp500 <- read.csv('1-sp500.csv', header=TRUE)
ret   <- diff(log(sp500$Close))
N     <- length(ret)


# Filtered Historical simulation
library('fGarch')
var1_hs  <- numeric(N) #historical simulation
var1_fhs  <- numeric(N)#filtered historical simulation
var1_garch <- numeric(N)#garch version
var1_t   <- numeric(N)#student T version

for (i in 501:N){
  retwindow   <- ret[(i-500):(i-1)]
  var1_hs[i] <- -quantile(retwindow, probs=0.01)
  
  fit2 <- garchFit( formula = ~garch(1, 1), data = retwindow, trace = FALSE)
  omega <- fit2@fit$par[2]
  alpha <- fit2@fit$par[3]
  beta  <- fit2@fit$par[4]
  sigma <- fit2@sigma.t
  sigmapred <- sqrt(omega + alpha*ret[500]^2 + beta*sigma[500]^2)
  retstand <- retwindow/sigma
  var1_fhs[i]  <- -sigmapred*quantile(retstand, probs=0.01)
  
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


#step 2: backtesting unconditional coverage test
violation <- numeric(N-500)
ind       <- which(ret[501:N] <= -var1_t[501:N])#change to hs.fhs.garch.t;backtesting student t verstion
violation[ind] <- 1 #violation at those location change to 1
plot(violation)
sum(violation)/length(violation)


T  <- length(violation)
T1 <- sum(violation)
T0 <- T - T1
pii <- T1/T #pi hat; estimate coverage rate

Lp <- (1-0.01)^T0*0.01^T1
Lpii <- (1-pii)^T0*pii^T1

LR  <- -2*log(Lp/Lpii)
LR
1-pchisq(LR, df=1)


#additional 1: Get the cutoff value for the Chi-squared distribution
qchisq(0.95, df=1)#q means quantile


#additional 2: backtest_coverage function;
#(x,p) are the inputs of the function. x: hit sequence; P: coverage rate
#outputs:(LR,Pii)
backtest_coverage <- function(x, p){
  T <- length(x)
  T1 <- sum(x)
  T0 <- T - T1
  pii <- T1/T
  
  Lp <- (1-p)^T0*p^T1
  Lpii <- (1-pii)^T0*pii^T1
  
  LR  <- -2*log(Lp/Lpii)
  c(LR, pii)
}


backtest_coverage(violation, 0.01)
#when the estimate coverage rate=pi hat too large or too low, it will give you larger LR>3.84,which means fail the unconditional coverage test


#step 2-2: independent testing(step one is the same)
violation <- numeric(N-500)
ind       <- which(ret[501:N] <= -var1_t[501:N])
violation[ind] <- 1
plot(violation, type='l')
pii <- sum(violation)/length(violation)


T  <- length(violation)
violation <- violation[1:(T-1)]*10 +  violation[2:T]
T00 <- length(which(violation==0))
T01 <- length(which(violation==1))
T10 <- length(which(violation==10))
T11 <- length(which(violation==11))
pii01 <- T01/(T00 + T01)
pii11 <- T11/(T10 + T11)

Lpii <- (1-pii)^T00*pii^T01*(1-pii)^T10*pii^T11#null
Lpii1 <- (1-pii01)^T00*pii01^T01*(1-pii11)^T10*pii11^T11#alternative

LR  <- -2*log(Lpii/Lpii1)
LR
1-pchisq(LR, df=1)

##all 4 test pass lower the better,but garch and student t are better than  hs and fhs
#hs is slowing movement,thus more pii11, 



















































