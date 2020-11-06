# -*- coding: utf-8 -*-
"""
Created on Wed Oct 28 23:38:36 2020

@author: lenovo
"""


import numpy as np
import pandas as pd
from scipy.stats import norm
import matplotlib.pyplot as plt
import math
from math import sqrt
from mpl_toolkits import mplot3d

df = pd.read_csv(r'E:\uconn\S4\5344\a-FNCE5344_2020F_Asgn1-Params.csv')


a=df.iloc[0,1]
b=df.iloc[1,1]
c=df.iloc[2,1]
d=df.iloc[3,1]
e=df.iloc[4,1]
f=df.iloc[5,1]
g=df.iloc[6,1]
h=df.iloc[7,1]
m=df.iloc[8,1]



def vasicek1(mu, kappa, sigma, r0, N, dt, T):
    I=int(T/dt)
    N=int(N)
    r=np.random.random((N,I+1))
    w=norm.ppf(r)
    rates=np.zeros((N,I+1))
    n=1
    i=1
    for n in range(N):
        rates[n,0]=r0
        for i in range(I):
            rates[n,i+1] = kappa*mu*dt + (1-mu*dt)*rates[n,i]+sigma*sqrt(dt)*w[n,i]
    return rates

if __name__ == "__main__":
    rates = vasicek1(a,b,c,d,e,f,g)


print(rates)






#question 3

trans = np.transpose(rates)

ra = trans[:,0]

for i in range(1,10):
    ra = np.concatenate((ra,trans[:,i]))



def vasicek3(mu, kappa, sigma, r0, N, dt, T, dtau, tau):
    I=int(T/dt)
    N=int(N)
    J=int(tau/dtau)
    r=np.random.random((N,I+1))
    w=norm.ppf(r)
    rates=np.zeros((N,I+1))
    z=np.zeros((N*(I+1),(J+3)))
    x=0
    y=0
    Btau=(1-math.exp(-kappa*tau))/kappa
    Atau=math.exp((Btau-tau)*(kappa*kappa*mu-sigma*sigma/2)/(kappa*kappa)
               -(sigma*sigma*Btau*Btau)/(4*kappa))

# 0 scen
    for x in range(N*(I+1)):       
        z[x,0]=int(x/(I+1))+1
        z[x,1]=dt*(x%21)
        z[x,2]=ra[x]
     

    for x in range(N*(I+1)):
        for y in range(3,(J+3)):
            z[x,y]=Atau*math.exp(-Btau*z[x,2])
        
    return z

if __name__ == "__main__":
    z=vasicek3(a,b,c,d,e,f,g,h,m)

z=pd.DataFrame(z)
col_names=['scen','time','short rate','price tau1','price tau2','price tau3','price tau4',
           'price tau5','price tau6','price tau7','price tau8','price tau9','price tau10',
           'price tau11','price tau12','price tau13','price tau14','price tau15','price tau16',
           'price tau17','price tau18','price tau19','price tau20']

z.columns=col_names
 
print(z)

z.to_csv(r'E:\uconn\S4\5344\a-data.csv',index=False)