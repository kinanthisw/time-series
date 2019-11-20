# ARIMA model for Google Trends data

library(lmtest)
library(tseries)
library(forecast)

setwd("E:/AD/Syntax AD")
data1 <- read.csv("GrabGojek1.csv", sep=",", header=T)
y1t <- as.ts(data1[,2])

#READ THE DATA FROM TXT#
Ytrain=as.ts(y1t[1:144])                             #define training data
Ytest=as.ts(y1t[145:168])                             #define testing data

#TIME SERIES PLOT#
par(mfrow=c(1,1),mar=c(3.1,3.2,1,0.3),mgp=c(2.1,0.5,0))
plot(Ytrain,axes=F,ylab="Yt", xlab="Hour")
box()
axis(side=2,lwd=0.5,cex.axis=0.7,las=2,cex=0.5)
axis(side=1,at=seq(1,144,24),lwd=0.5,cex.axis=0.8,las=0)
points(Ytrain,col="red3",cex=0.75,pch=19)

#CHECKING FOR STATIONARY USING ACF PLOT#
tick=c(1,24,48,72)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))    #the number of picture and its margin
par(mgp=c(1.7,0.5,0))                     #the distance between labels and axis
#ACF
acf(Ytrain,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
abline(v=tick,lty="dotted", lwd=2, col="grey")
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
#PACF
pacf(Ytrain,lag.max=72,axes=F, ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
abline(v=tick,lty="dotted", lwd=2, col="grey")
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#CHECKING FOR STATIONARY USING ADF TEST#
#trunc((length(Ytrain)-1)^(1/3))
adf.test(Ytrain, k=24)

#DIFFERENCING SEASONAL ORDER FOR YTRAIN
Wtrain=diff(Ytrain,lag=24)
par(mfrow=c(1,1))
plot(Wtrain)

#CHECKING FOR STATIONARY USING ADF TEST#
#trunc((length(Ytrain)-1)^(1/3))
adf.test(Wtrain, k=10)

#DIFFERENCING NON-SEASONAL ORDER FOR WTRAIN
Ztrain=diff(Wtrain,lag=1)
par(mfrow=c(1,1))
plot(Ztrain)
adf.test(Ztrain, k=10)

#ORDER IDENTIFICATION USING ACF AND PACF FROM STATIONARY DATA
tick=c(1,24,48,72)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))    #the number of picture and its margin
par(mgp=c(1.7,0.5,0))                     #the distance between labels and axis
#ACF
acf(Ztrain,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
#abline(v=tick,lty="dotted", lwd=2, col="pink")
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
#PACF
pacf(Ztrain,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
#abline(v=tick,lty="dotted", lwd=2, col="pink")
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)


###################################################  
#1st ARIMA model
#seasonal ARIMA model (meet white noise assumption)
modelARIMA=arima(Ytrain, order = c(4,1,0),
                 seasonal = list(order = c(1,1,0),
                                 period =24),
                 include.mean=TRUE, method = c("ML"))
summary(modelARIMA)                                        #ARIMA (0,0,0)(0,1,0)12
coeftest(modelARIMA)                                       #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)                     #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))                       #define forecast value for training data


#2nd ARIMA model
#seasonal ARIMA model (meet white noise assumption)
modelARIMA=arima(Ytrain, order = c(20,1,0),
                 seasonal = list(order = c(1,1,0),period =24),
                 transform.pars = FALSE, 
                 fixed=c(NA,NA,NA,NA,rep(0,15),NA,NA),                          #NA was the estimated lag, that is : 23
                 include.mean=TRUE, method = c("ML"))


summary(modelARIMA)                                        #ARIMA (0,0,0)(0,1,0)12
coeftest(modelARIMA)                                       #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)                     #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))                       #define forecast value for training data


#3rd ARIMA model
#seasonal ARIMA model (meet white noise assumption)
modelARIMA=arima(Ytrain, order = c(4,1,20),
                 seasonal = list(order = c(1,1,0),period =24),
                 transform.pars = FALSE, 
                 fixed=c(NA,NA,NA,NA,rep(0,19),NA,NA),                          #NA was the estimated lag, that is : 23
                 include.mean=TRUE, method = c("ML"))


summary(modelARIMA)                                        #ARIMA (0,0,0)(0,1,0)12
coeftest(modelARIMA)                                       #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)                     #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))                       #define forecast value for training data



par(mfrow=c(1,1))
plot(Ytrain)
lines(fits.ARIMA, col="red")

fore.ARIMA=predict(fits.ARIMA, 24)$pred                    #define forecast value for testing data
se.fore.ARIMA=predict(fits.ARIMA, 24)$se  

#DIAGNOSTIC CHECKING FOR ARIMA MODEL
#Independency test by using Ljung-Box test
lags <- c(6,12,18,24,30,36,42,48)                     #lag we used
p=6                                                   #the number of ar parameter
q=0                                                   #the number of ma parameter
LB.result<-matrix(0,length(lags),2)
for(i in seq_along(lags))
{
  LB.test=Box.test (resi.ARIMA, lag = lags[i],type = c("Ljung-Box"),fitdf=p+q)
  LB.result[i,1]=LB.test$statistic
  LB.result[i,2]=LB.test$p.value
}
rownames(LB.result)<-lags
colnames(LB.result)<-c("statistics","p.value")
LB.result

#ACF and PACF for RESIDUAL ARIMA MODEL
tick=c(1,24,48,72)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))    #the number of picture and its margin
par(mgp=c(1.7,0.5,0))                     #the distance between labels and axis
#ACF
acf(resi.ARIMA,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
#abline(v=tick,lty="dotted", lwd=2, col="grey")
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
#PACF
pacf(resi.ARIMA,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
#abline(v=tick,lty="dotted", lwd=2, col="grey")
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#Normality test using Kolmogorov Smirnov
ks.test(resi.ARIMA,"pnorm",mean=mean(resi.ARIMA),sd=sd(resi.ARIMA))

#FORECAST FOR TESTING DATA
fore.ARIMA=predict(modelARIMA, 24)$pred        #define forecast value for testing data
se.fore.ARIMA=predict(modelARIMA, 24)$se       #define standard error for forecasting result

#CONSTRUCT INTERVAL PREDICTION
lower=fore.ARIMA-1.96*se.fore.ARIMA
upper=fore.ARIMA+1.96*se.fore.ARIMA

#COMPARISON BETWEEN ACTUAL AND FORECAST VALUE
a=min(min(fits.ARIMA),min(Ytrain))              #lower bound for training data
b=max(max(fits.ARIMA),max(Ytrain))              #upper bound for training data
c=min(min(fore.ARIMA),min(lower),min(Ytest))    #lower bound for testing data
d=max(max(fore.ARIMA),max(upper),max(Ytest))    #upper bound for testing data

par(mfrow=c(1,2),mar=c(2.3,2.7,1.2,0.4))  #the number of picture and its margin
par(mgp=c(1.3,0.5,0))                     #the distance between labels and axis

#PLOTTING FOR TRAINING DATA#
plot(as.ts(Ytrain),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*0.9,b*1.1))
box()
title("Training",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=0)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=seq(1,144,24))
lines(as.ts(fits.ARIMA),col="red",lwd=2)

#PLOTTING FOR TESTING DATA#
plot(as.ts(Ytest),ylab="Yt",xlab="t",lwd=2,ylim=c(a*0.9,b*1.1),cex.lab=0.8,axes=F)
box()
title("Testing",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=0)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:24),labels=c(145:168))
lines(as.vector(fore.ARIMA),col="red",lwd=2)
lines(as.vector(lower),col="blue2",lty="dotdash",lwd=2)
lines(as.vector(upper),col="blue2",lty="dotdash",lwd=2)

#DEFINE THE LEGEND#
legend("topright",c("Actual","Forecast","Upper Bound","Lower Bound"),
       col=c("black","red","blue2","blue2"),lwd=2,cex=0.6)

#CALCULATE RMSE, MAE, AND MAPE CRITERIA
accuracies=matrix(0,3,2)
colnames(accuracies)=c("Training","Testing")
rownames(accuracies)=c("RMSE","MAE","MAPE")

accuracies[1,1]=accuracy(fits.ARIMA,Ytrain)[1,2]
accuracies[2,1]=accuracy(fits.ARIMA,Ytrain)[1,3]
accuracies[3,1]=accuracy(fits.ARIMA,Ytrain)[1,5]
accuracies[1,2]=accuracy(as.vector(fore.ARIMA),Ytest)[1,2]
accuracies[2,2]=accuracy(as.vector(fore.ARIMA),Ytest)[1,3]
accuracies[3,2]=accuracy(as.vector(fore.ARIMA),Ytest)[1,5]
accuracies
