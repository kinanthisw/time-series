library(neuralnet)
library(forecast)

#MEMBACA DATA#
setwd("E:/AD/Syntax AD")
data1 <- read.csv("GrabGojek1.csv", sep=",", header=T)
y1t <- as.ts(data1[,2])
training=as.ts(y1t[1:144])     #membagi menjadi data training
testing=as.ts(y1t[145:168])    #membagi menjadi data testing

#IDENTIFIKASI INPUT MENGGUNAKAN LAGPLOT
lag.plot(training, lags=30)

#PREPROCESSING STANDARDIZED#
mean.Yt = mean(training)
sd.Yt   = sd(training)
Yt_std <- scale(y1t)
Yt_std1 <- as.data.frame(scale(y1t))
library(DataCombine)
data1=slide(Yt_std1, Var = "V1", slideBy = -c(1,2,3,22,23,24,25,26))
colnames(data1)=c("yt","yt1","yt2","yt3","yt22","yt23","yt24","yt25","yt26")
data=data1[27:144,]
datatest=data1[145:168,]

#MEMBENTUK MODEL NEURAL NETWORK#
neuron=c(1:5)                       #neuron yang akan digunakan
neuron1=c(1)
n_fore=24                             #forecast berapa periode kedepan
seed=c(1,3,3,1,5,2,2)                #berdasarkan percobaan beberapa set.seed yang berbeda

best.model_NN=list
fits.model_NN=matrix(0,nrow(data),length(neuron)*length(neuron1))
fore.model_NN=matrix(0,n_fore,length(neuron)*length(neuron1))
letak=matrix(c(1:5),nrow=5,ncol=1)

allvars=colnames(data)
predictorvarss=allvars[!allvars%in%"yt"]
predictorvarss=paste(predictorvarss,collapse = "+")
form=as.formula(paste("yt~",predictorvarss,collapse="+"))

for (k in seq_along(neuron))
{
  for (j in seq_along(neuron1))
  {
  set.seed(1)
  best.model_NN=neuralnet(formula=form,data=data,hidden=c(neuron[k],neuron1[j]),
                          act.fct="tanh",linear.output=TRUE,likelihood=TRUE,stepmax = 1000000)
  a=letak[k,j]
  best.model_NN[[a]]=best.model_NN
  fits.model_NN[,a]=(as.ts(unlist(best.model_NN[[a]]$net.result)))*sd.Yt+mean.Yt  #hasil ramalan data training
  
  #ARSITEKTUR NEURAL NETWORK#
  win.graph()
  plot(best.model_NN[[a]])
  
  #FORECAST k-STEP AHEAD#
  Ytest=c(data[,1],rep(0,n_fore))
  for(i in (nrow(data)+1):(nrow(data)+n_fore))
  {
    Xtest=datatest[,2:ncol(datatest)]
    Ytest[i]=compute(best.model_NN[[a]],Xtest)$net.result[i-nrow(data)]
  }
  fore.model_NN[,a]=Ytest[(nrow(data)+1):(nrow(data)+n_fore)]*sd.Yt+mean.Yt       #hasil ramalan data testing
}
}


#MEMBERI NAMA KOLOM UNTUK MATRIKS HASIL FORECAST#
colnames(fore.model_NN)=c("Neuron 1","Neuron 2","Neuron 3","Neuron 4",
                          "Neuron 5") 

#MENGHITUNG TINGKAT KESALAHAN PERAMALAN#
akurasi=matrix(0,length(neuron),6)
colnames(akurasi)=c("RMSE_training","MAE_training","MAPE_training",
                    "RMSE_testing","MAE_testing","MAPE_testing")
rownames(akurasi)=c("Neuron 1","Neuron 2","Neuron 3","Neuron 4",
                    "Neuron 5")
for (i in 1:length(neuron))
{
  akurasi[i,1]=accuracy(fits.model_NN[,i],training[27:144])[1,2]
  akurasi[i,2]=accuracy(fits.model_NN[,i],training[27:144])[1,3]
  akurasi[i,3]=accuracy(fits.model_NN[,i],training[27:144])[1,5]
  akurasi[i,4]=accuracy(fore.model_NN[,i],testing)[1,2]
  akurasi[i,5]=accuracy(fore.model_NN[,i],testing)[1,3]
  akurasi[i,6]=accuracy(fore.model_NN[,i],testing)[1,5]
}

akurasi

#MEMBUAT PLOT PERBANDINGAN RMSE, MAE, MAPE#
RMSE_a=min(c(akurasi[,1],akurasi[,4]))        #batas bawah plot RMSE
RMSE_b=max(c(akurasi[,1],akurasi[,4]))        #batas atas plot RMSE
MAE_a=min(c(akurasi[,2],akurasi[,5]))         #batas bawah plot MAE
MAE_b=max(c(akurasi[,2],akurasi[,5]))         #batas atas plot MAE
MAPE_a=min(c(akurasi[,3],akurasi[,6]))        #batas bawah plot MAPE
MAPE_b=max(c(akurasi[,3],akurasi[,6]))        #batas atas plot MAPE



par(mfrow=c(3,1),mar=c(2.7,2.9,1.2,0.4))  #banyaknya gambar dan ukuran margin
par(mgp=c(1.7,0.5,0))                     #jarak judul label ke axis

#RMSE
plot(as.ts(akurasi[,1]),ylab="RMSE",xlab="Neuron",lwd=2,axes=F,ylim=c(RMSE_a*1.1,RMSE_b*1.1))
box()
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:5),labels=neuron)
lines(akurasi[,4],col="red2",lwd=2)
legend("topright",c("Data training","Data testing"),
       col=c("black","red2"),
       lwd=2,cex=0.7)
#MAE
plot(as.ts(akurasi[,2]),ylab="MAE",xlab="Neuron",lwd=2,axes=F,ylim=c(MAE_a*1.1,MAE_b*1.1))
box()
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:10),labels=neuron)
lines(akurasi[,5],col="red2",lwd=2)
legend("topright",c("Data training","Data testing"),
       col=c("black","red2"),
       lwd=2,cex=0.7)
#MAPE
plot(as.ts(akurasi[,3]),ylab="MAPE",xlab="Neuron",lwd=2,axes=F,ylim=c(MAPE_a*1.1,MAPE_b*1.1))
box()
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:10),labels=neuron)
lines(akurasi[,6],col="red2",lwd=2)
legend("topright",c("Data training","Data testing"),
       col=c("black","red2"),
       lwd=2,cex=0.7)

#MEMBUAT PLOT PERBANDINGAN DATA AKTUAL DAN RAMALAN SEMUA NEURON#
a=min(min(fits.model_NN),min(training[27:144]))   #batas bawah plot data training
b=max(max(fits.model_NN),max(training[27:144]))   #batas atas plot data training
c=min(min(fore.model_NN),min(testing))    #batas bawah plot data testing
d=max(max(fore.model_NN),max(testing))    #batas atas plot data testing
#colors()                                  #warna yang tersedia di R

par(mfrow=c(1,2),mar=c(2.3,2.7,1.2,0.4))  #banyaknya gambar dan ukuran margin
par(mgp=c(1.3,0.5,0))                     #jarak judul label ke axis
warna=c("red2","blue2","pink2","green3","grey88","yellow2","skyblue")

#PLOT DATA TRAINING#
plot(as.ts(training[27:144]),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*1.1,b*1.1))
box()
title("Data training",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1,seq(35,400,35)))
for (i in 1:length(neuron)*length(neuron1))
{lines(as.ts(fits.model_NN[,i]),col=warna[i],lwd=2)}

#PLOT DATA TESTING#
plot(as.ts(testing),ylab="Yt",xlab="t",lwd=2,ylim=c(c*1.1,d*1.2),cex.lab=0.8,axes=F)
box()
title("Data testing",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:24),labels=c(145:168))
for (i in 1:length(neuron))
{lines(as.ts(fore.model_NN[,i]),col=warna[i],lwd=2)}

#MEMBERI NAMA LEGEND#
legend("topright",c("Data aktual","Neuron 1","Neuron 2","Neuron 3","Neuron 4",
                    "Neuron 5","Neuron 6","Neuron 7","Neuron 8","Neuron 9","Neuron 10"),
       col=c("black",warna),
       lwd=2,cex=0.7)

#MEMBUAT PLOT PERBANDINGAN DATA AKTUAL DAN RAMALAN NEURON 1,5,10,15#
a=min(min(fits.model_NN),min(training[27:144]))   #batas bawah plot data training
b=max(max(fits.model_NN),max(training[27:144]))   #batas atas plot data training
c=min(min(fore.model_NN),min(testing))    #batas bawah plot data testing
d=max(max(fore.model_NN),max(testing))    #batas atas plot data testing
colors()                                  #warna yang tersedia di R

par(mfrow=c(2,2),mar=c(2.3,2.7,1.2,0.4))  #banyaknya gambar dan ukuran margin
par(mgp=c(1.3,0.5,0))                     #jarak judul label ke axis

#PLOT DATA TRAINING#
#Neuron 1#
plot(as.ts(training[27:144]),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*1.1,b*1.5))
box()
title("Data training neuron 1",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1,seq(35,400,35)))
lines(as.ts(fits.model_NN[,1]),col="red2",lwd=2)
legend("topleft",c("Data aktual","Data ramalan"),
       col=c("black","red2"),lwd=2,cex=0.7)
#Neuron 5#
plot(as.ts(training[8:393]),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*1.1,b*1.5))
box()
title("Data training neuron 5",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1,seq(35,400,35)))
lines(as.ts(fits.model_NN[,5]),col="grey88",lwd=2)
legend("topleft",c("Data aktual","Data ramalan"),
       col=c("black","grey88"),lwd=2,cex=0.7)
#Neuron 10#
plot(as.ts(training[8:393]),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*1.1,b*1.5))
box()
title("Data training neuron 10",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1,seq(35,400,35)))
lines(as.ts(fits.model_NN[,6]),col="yellow2",lwd=2)
legend("topleft",c("Data aktual","Data ramalan"),
       col=c("black","yellow2"),lwd=2,cex=0.7)
#Neuron 15#
plot(as.ts(training[8:393]),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*1.1,b*1.5))
box()
title("Data training neuron 15",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1,seq(35,400,35)))
lines(as.ts(fits.model_NN[,7]),col="skyblue",lwd=2)
legend("topleft",c("Data aktual","Data ramalan"),
       col=c("black","skyblue"),lwd=2,cex=0.7)

#PLOT DATA TESTING#
#Neuron 1#
plot(as.ts(testing),ylab="Yt",xlab="t",lwd=2,ylim=c(c*1.1,d*1.5),cex.lab=0.8,axes=F)
box()
title("Data testing neuron 1",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:24),labels=c(145:168))
lines(as.ts(fore.model_NN[,1]),col="red2",lwd=2)
legend("topleft",c("Data aktual","Data ramalan"),
       col=c("black","red2"),lwd=2,cex=0.7)
#Neuron 5#
plot(as.ts(testing),ylab="Yt",xlab="t",lwd=2,ylim=c(c*1.1,d*1.5),cex.lab=0.8,axes=F)
box()
title("Data testing neuron 5",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:7),labels=c(394:400))
lines(as.ts(fore.model_NN[,5]),col="grey88",lwd=2)
legend("topleft",c("Data aktual","Data ramalan"),
       col=c("black","grey88"),lwd=2,cex=0.7)
#Neuron 10#
plot(as.ts(testing),ylab="Yt",xlab="t",lwd=2,ylim=c(c*1.1,d*1.5),cex.lab=0.8,axes=F)
box()
title("Data testing neuron 10",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:7),labels=c(394:400))
lines(as.ts(fore.model_NN[,6]),col="yellow2",lwd=2)
legend("topleft",c("Data aktual","Data ramalan"),
       col=c("black","yellow2"),lwd=2,cex=0.7)
#Neuron 15#
plot(as.ts(testing),ylab="Yt",xlab="t",lwd=2,ylim=c(c*1.1,d*1.5),cex.lab=0.8,axes=F)
box()
title("Data testing neuron 15",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=2)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:7),labels=c(394:400))
lines(as.ts(fore.model_NN[,7]),col="skyblue",lwd=2)
legend("topleft",c("Data aktual","Data ramalan"),
       col=c("black","skyblue"),lwd=2,cex=0.7)
