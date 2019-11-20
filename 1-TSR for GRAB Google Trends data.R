# Setting the folder for practical work
# Use library(forecast) for running 'accuracy'
#LOAD THE PACKAGE TO MAKE DUMMY VARIABLES

library(forecast)
library(dummies)

setwd("E:/AD/Syntax AD")
data1 <- read.csv("GrabGojek1.csv", sep=",", header=T)
dim(data1)
head(data1)

y1t <- as.ts(data1[,2])
plot(y1t)

#DUMMY VARIABLES FOR TREND
Dummy.T <- c(1:168)
head(Dummy.T)
tail(Dummy.T)

#DUMMY VARIABLES FOR Hour
Hour1 <- data.frame(Hour = rep(1:24,8))
Hour <- data.frame(Hour = Hour1[8:175, ])
head(Hour)
tail(Hour)
#Hour <- Hour[1:168,2]
Dummy.M <- dummy(Hour$Hour, sep = "_")
dim(Dummy.M)
Dummy.M <- Dummy.M[1:168, ]
dim(Dummy.M)
head(Dummy.M)

#DEFINE VARIABLE FOR DUMMY

Dummy = cbind(Dummy.T, Dummy.M)
dim(Dummy)
head(Dummy)

# TIME SERIES REGRESSION MODELLING#
Ytrain = as.ts(y1t[1:144])                #define training data
Ytest = as.ts(y1t[145:168])               #define testing data
x=as.matrix(Dummy[1:144, ])               #define predictor variables (dummy: trend, seasonal, and calendar variation)

#### Time Series model #####
modelTSR=lm(Ytrain ~ x-1)                 #modelling using TSR
summary(modelTSR)
#### Time Series model #####

resi.TSR=as.ts(modelTSR$residuals)        #define residual value
fits.TSR=as.ts(modelTSR$fitted.values)    #define forecast value for training data


#FORECAST FOR TESTING DATA
x=as.matrix(Dummy[145:168, ])                           #define predictor variables
new=data.frame(x=as.matrix(Dummy[145:168, ]))
fore.TSR=predict(modelTSR,new,se.fit=TRUE)$fit          #define forecast value for testing data
se.fore.TSR=predict(modelTSR,new,se.fit=TRUE)$se.fit    #define standard error for forecasting result

#CONSTRUCT INTERVAL PREDICTION
lower=fore.TSR-1.96*se.fore.TSR
upper=fore.TSR+1.96*se.fore.TSR

#COMPARISON BETWEEN ACTUAL AND FORECAST VALUE
a=min(min(fits.TSR),min(Ytrain))              #lower bound for training data
b=max(max(fits.TSR),max(Ytrain))              #upper bound for training data
c=min(min(fore.TSR),min(lower),min(Ytest))    #lower bound for testing data
d=max(max(fore.TSR),max(upper),max(Ytest))    #upper bound for testing data

par(mfrow=c(1,2),mar=c(2.3,2.7,1.2,0.4))  #the number of picture and its margin
par(mgp=c(1.3,0.5,0))                     #the distance between labels and axis

#PLOT TRAINING DATA#
plot(as.ts(Ytrain),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*0.9,b*1.1))
box()
title("Training",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=seq(1,144,24))
lines(as.ts(fits.TSR),col="red",lwd=2)

#PLOT TESTING DATA#
plot(as.ts(Ytest),ylab="Yt",xlab="t",lwd=2,ylim=c(a*0.9,b*1.1),cex.lab=0.8,axes=F)
box()
title("Testing",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:24),labels=c(145:168))
lines(as.ts(fore.TSR),col="red",lwd=2)
lines(as.ts(lower),col="blue2",lty="dotdash",lwd=2)
lines(as.ts(upper),col="blue2",lty="dotdash",lwd=2)

#DEFINE THE LEGEND#
legend("topright",c("Actual","Forecast","Upper Bound","Lower Bound"),
       col=c("black","red","blue2","blue2"),lwd=2,cex=0.6)

#DIAGNOSTIC CHECKING FOR ARIMA MODEL
#Independency test by using Ljung-Box test
lags <- c(6,12,18,24,30,36,42,48)                     #lag we used
p=0                                                   #the number of ar parameter
q=0                                                   #the number of ma parameter
LB.result<-matrix(0,length(lags),2)
for(i in seq_along(lags))
{
  LB.test=Box.test (resi.TSR, lag = lags[i],type = c("Ljung-Box"),fitdf=p+q)
  LB.result[i,1]=LB.test$statistic
  LB.result[i,2]=LB.test$p.value
}
rownames(LB.result)<-lags
colnames(LB.result)<-c("statistics","p.value")
LB.result

#ACF and PACF for RESIDUAL ARIMA MODEL
tick=c(1,12,24,36)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))    #the number of picture and its margin
par(mgp=c(1.7,0.5,0))                     #the distance between labels and axis
#ACF
acf(resi.TSR,lag.max=36,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
#abline(v=tick,lty="dotted", lwd=2, col="grey")
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
#PACF
pacf(resi.TSR,lag.max=36,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
#abline(v=tick,lty="dotted", lwd=2, col="grey")
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#Normality test using Kolmogorov Smirnov
ks.test(resi.TSR,"pnorm",mean=mean(resi.TSR),sd=sd(resi.TSR))

#Need to run library(forecast) 
#CALCULATE RMSE, MAE, AND MAPE CRITERIA
accuracies=matrix(0,3,2)
colnames(accuracies)=c("Training","Testing")
rownames(accuracies)=c("RMSE","MAE","MAPE")

accuracies[1,1]=accuracy(fits.TSR,Ytrain)[1,2]
accuracies[2,1]=accuracy(fits.TSR,Ytrain)[1,3]
accuracies[3,1]=accuracy(fits.TSR,Ytrain)[1,5]
accuracies[1,2]=accuracy(fore.TSR,Ytest)[1,2]
accuracies[2,2]=accuracy(fore.TSR,Ytest)[1,3]
accuracies[3,2]=accuracy(fore.TSR,Ytest)[1,5]
accuracies

