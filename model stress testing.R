library(MASS)
a=read.csv("E:/uconn/S1/project/stress testing/DJIA.csv",header=T)
b=read.csv("E:/uconn/S1/project/stress testing/1 year treasury.csv",header=T)
c=read.csv("E:/uconn/S1/project/stress testing/3 month treasury.csv",header=T)
d=read.csv("E:/uconn/S1/project/stress testing/3-Month AA Financial Commercial Paper Rate.csv",header=T)
e=read.csv("E:/uconn/S1/project/stress testing/5-Year Treasury Constant Maturity Rate.csv",header=T)
f=read.csv("E:/uconn/S1/project/stress testing/10-Year Treasury Constant Maturity Rate.csv",header=T)
g=read.csv("E:/uconn/S1/project/stress testing/SP500.csv",header=T)
h=read.csv("E:/uconn/S1/project/stress testing/ICE BofAML Emerging Markets Corporate Plus Index Total Return Index Value.csv",header=T)
i=read.csv("E:/uconn/S1/project/stress testing/ICE BofAML US High Yield Master II Effective Yield.csv",header=T)

attach(a)
attach(b)
attach(c)
attach(d)
attach(e)
attach(f)
attach(g)
attach(h)
attach(i)

dim(a)
dim(b)
dim(c)
dim(d)
dim(e)
dim(f)
dim(g)
dim(h)
dim(i)

names(M)


M<-Reduce(function(x,y)merge(x,y,all=T),list(a,b,c,d,e,f,g,h,i))
M$DJIA[which(M$DJIA=='.')]='NA'
M$DGS1[which(M$DGS1=='.')]='NA'
M$DGS3MO[which(M$DGS3MO=='.')]='NA'
M$DCPF3M[which(M$DCPF3M=='.')]='NA'
M$DGS5[which(M$DGS5=='.')]='NA'
M$DGS10[which(M$DGS10=='.')]='NA'
M$SP500[which(M$SP500=='.')]='NA'
M$BAMLEMCBPITRIV[which(M$BAMLEMCBPITRIV=='.')]='NA'
M$BAMLH0A0HYM2EY[which(M$BAMLH0A0HYM2EY=='.')]='NA'
M=na.omit(M)
dim(M)

M$BAMLH0A0HYM2EY=as.numeric(M$BAMLH0A0HYM2EY)
M$DGS3MO=as.numeric(M$DGS3MO)
M$DGS5=as.numeric(M$DGS5)
M$DGS10=as.numeric(M$DGS10)
M$SP500=as.numeric(M$SP500)
M$DGS1=as.numeric(M$DGS1)
M$DCPF3M=as.numeric(M$DCPF3M)
M$BAMLEMCBPITRIV=as.numeric(M$BAMLEMCBPITRIV)
M$DJIA=as.numeric(M$DJIA)

names(M)
M$DGS1=M$DGS1/100
M$DGS3MO=M$DGS3MO/100
M$DCPF3M=M$DCPF3M/100
M$DGS5=M$DGS5/100
M$DGS10=M$DGS10/100
M$BAMLH0A0HYM2EY=M$BAMLH0A0HYM2EY/100

names(d)
M$DCPF3M

class(M$DJIA)

ra=rep(0,2472)
for (i in 1:2471) {
  ra[i]=(M$DJIA[i+1]-M$DJIA[i])/M$DJIA[i]
}
ra

re=rep(0,2472)
rf=rep(0,2472)
rg=rep(0,2472)
rc=rep(0,2472)
rb=rep(0,2472)
rd=rep(0,2472)
rh=rep(0,2472)
ri=rep(0,2472)

rg=rep(0,2472)
for (i in 1:2471) {
  rg[i]=(M$SP500[i+1]-M$SP500[i])/M$SP500[i]
}
rg

rc=rep(0,2472)
for (i in 1:2471) {
  rc[i]=-0.25*(M$DGS3MO[i+1]-M$DGS3MO[i])
}
rc

rf=rep(0,2472)
for (i in 1:2471) {
  rf[i]=-10*(M$DGS10[i+1]-M$DGS10[i])
}
rf

re=rep(0,2472)
for (i in 1:2471) {
  re[i]=-5*(M$DGS5[i+1]-M$DGS5[i])
}
re

names(b)
rb=rep(0,2472)
for (i in 1:2471) {
  rb[i]=-1*(M$DGS1[i+1]-M$DGS1[i])
}
rb

rd=rep(0,2472)
for (i in 1:2471) {
  rd[i]=-0.25*(M$DCPF3M[i+1]-M$DCPF3M[i])
}
rd

rh=rep(0,2472)
for (i in 1:2471) {
  rh[i]=(M$BAMLEMCBPITRIV[i+1]-M$BAMLEMCBPITRIV[i])/M$BAMLEMCBPITRIV[i]
}
rh

names(i)
ri=rep(0,2472)
for (i in 1:2471) {
  ri[i]=-(M$BAMLH0A0HYM2EY[i+1]-M$BAMLH0A0HYM2EY[i])
}
ri

#variance
var(ra)
mean(ra)
var(rb)
mean(rb)
var(rc)
mean(rc)
var(rd)
mean(rd)
var(re)
mean(re)
var(rf)
mean(rf)
var(rg)
mean(rg)
var(rh)
mean(rh)
var(ri)
mean(ri)

#MODEL1
y1=rg
M1.train=data.frame(ra,re,rc,rf,y1)
M1.train=na.omit(M1.train)
dim(M1.train)
lm1=lm(y1~ra+re+rc+rf,data=M1.train)
summary(lm1)
y1=0.0120019-0.0008840*ra+0.1137210*re+0.7300676*rc-0.1400254*rf

#MODEL2
y2=0.6*rg+0.4*rh
M2.train=data.frame(y2,ra,rc,rf,re)
M2.train=na.omit(M2.train)
dim(M2.train)
lm2=lm(y2~ra+re+rc+rf,data=M2.train)
summary(lm2)
y2=0.0088301-0.0006429*ra+0.0770172*re+0.3263250*rc-0.0805023*rf

#MODEL3
y3=0.5*rg+0.5*rd
M3.train=data.frame(y3,ra,rc,rf,re)
M3.train=na.omit(M3.train)
dim(M3.train)
lm3=lm(y3~ra+re+rc+rf,data=M3.train)
summary(lm3)
y3=0.0060009-0.0004420*ra+0.0568605*re+0.3650338*rc-0.0700127*rf

#MODEL4
y4=0.5*rg+0.2*ri+0.3*rb
M4.train=data.frame(y4,ra,rc,rf,re)
M4.train=na.omit(M4.train)
dim(M4.train)
lm4=lm(y4~ra+re+rc+rf,data=M4.train)
summary(lm4)
y4=0.0062589-0.0004512*ra+0.0587302*re+1.1257419*rc-0.0667393*rf

#Q4 2021 Shock:3-month,5-year,10-year,Dow Jones
today=c(0.0154,0.0162,0.018,28015.06)
A3=c(0.028,0.034,0.035,29527)
A5=c(0.001,0.013,0.019,24621)

#baseline scenario
Dr=rep(0,4)
Dr[1]=(A3[1]-today[1])*(-0.25)
Dr[2]=(A3[2]-today[2])*(-5)
Dr[3]=(A3[3]-today[3])*(-10)
Dr[4]=(A3[4]-today[4])/today[4]
Dr
ra=Dr[4]
rc=Dr[1]
re=Dr[2]
rf=Dr[3]
y1.ba=0.0120019-0.0008840*ra+0.1137210*re+0.7300676*rc-0.1400254*rf
y2.ba=0.0088301-0.0006429*ra+0.0770172*re+0.3263250*rc-0.0805023*rf
y3.ba=0.0060009-0.0004420*ra+0.0568605*re+0.3650338*rc-0.0700127*rf
y4.ba=0.0062589-0.0004512*ra+0.0587302*re+1.1257419*rc-0.0667393*rf
y.baseline=c(y1.ba,y2.ba,y3.ba,y4.ba)
y.baseline

#adverse scenario
Dr2=rep(0,4)
Dr2[1]=(A5[1]-today[1])*(-0.25)
Dr2[2]=(A5[2]-today[2])*(-5)
Dr2[3]=(A5[3]-today[3])*(-10)
Dr2[4]=(A5[4]-today[4])/today[4]
ra=Dr2[4]
rc=Dr2[1]
re=Dr2[2]
rf=Dr2[3]
y1.ad=0.0120019-0.0008840*ra+0.1137210*re+0.7300676*rc-0.1400254*rf
y2.ad=0.0088301-0.0006429*ra+0.0770172*re+0.3263250*rc-0.0805023*rf
y3.ad=0.0060009-0.0004420*ra+0.0568605*re+0.3650338*rc-0.0700127*rf
y4.ad=0.0062589-0.0004512*ra+0.0587302*re+1.1257419*rc-0.0667393*rf
y_adverse=c(y1.ad,y2.ad,y3.ad,y4.ad)
y_adverse
y_baseline
#conclusion:in adverse scenario portfolio 1 has the highest return; in baseline scenario portfolio 4 has the highest return.

