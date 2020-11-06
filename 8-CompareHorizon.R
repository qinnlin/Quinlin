


#### As return horizon increases, its unconditional distribution looks increasingly like normal distribution.

rm(list=ls())                                  

dailyret   <- read.csv('dailySP500.csv', header=TRUE)         
monthlyret <- read.csv('monthlySP500.csv', header=TRUE)         
annualret  <- read.csv('annualSP500.csv', header=TRUE)  



mean(dailyret$ret)*100
sd(dailyret$ret)*100
mean(monthlyret$ret)*100
sd(monthlyret$ret)*100
mean(annualret$ret)*100
sd(annualret$ret)*100




par(mfrow=c(1,3))
qqnorm(dailyret$ret, xlim=c(-4,4))
qqline(dailyret$ret, col='red', lw=4)

qqnorm(monthlyret$ret, xlim=c(-4,4))
qqline(monthlyret$ret, col='red', lw=4)

qqnorm(annualret$ret, xlim=c(-4,4))
qqline(annualret$ret, col='red', lw=4)



#### Let's scale all returns to have a mean of zero and a standard deviation of one
par(mfrow=c(1,3))
qqnorm(scale(dailyret$ret), xlim=c(-4,4))
qqline(scale(dailyret$ret), col='red', lw=4)

qqnorm(scale(monthlyret$ret), xlim=c(-4,4))
qqline(scale(monthlyret$ret), col='red', lw=4)

qqnorm(scale(annualret$ret), xlim=c(-4,4))
qqline(scale(annualret$ret), col='red', lw=4)
