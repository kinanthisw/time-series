data(iris)
nama=as.matrix(iris[,5])



#simulasi data 1 (Tidak tumpang tindih & High correlated)
library(MASS)
library(matrixcalc)
library(corpcor)
Sigma=matrix(c(1,0.8,0.7,0.8,0.8,1,0.9,0.9,0.7,0.9,1,0.9,0.8,0.9,0.9,1),4,4)
Sigma
rank(Sigma)
is.positive.definite(Sigma)
Sigma1=make.positive.definite(Sigma)
is.positive.definite(Sigma1)
Muk1=c(10,10,10,10)
datak1=mvrnorm(n=50,Muk1,Sigma1)
Muk2=c(15,15,15,15)
datak2=mvrnorm(n=50,Muk2,Sigma1)
Muk3=c(20,20,20,20)
datak3=mvrnorm(n=50,Muk3,Sigma1)
data=rbind(datak1,datak2,datak3)
data=data.frame(data,nama)
colnames(data)=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
data=as.data.frame(data)

#Plot
library(ggplot2)
ggplot(data, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point()
my_cols = c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(data[,1:4], pch = 19,  cex = 0.7,
      col = my_cols[data$Species],
      lower.panel=NULL)
library(corrplot)
a=cor(data[,1:4])
corrplot(a, method = "color",
         type = "upper", order = "hclust", number.cex = .7,
        addCoef.col = "black", 
        tl.col = "black", tl.srt = 90)
library(factoextra)

fviz_nbclust(data[,1:4],FUN=hcut,method="wss")
   ab=dist(data[,1:4],method="euclidian")
clusters <- hclust(ab,method="complete")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 


clusters <- hclust(ab,method="average")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="single")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="ward.D")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 



#simulasi data 2 (Tumpang tindih & High correlated)
library(MASS)
library(matrixcalc)
library(corpcor)
Sigma=matrix(c(1,0.8,0.7,0.8,0.8,1,0.9,0.9,0.7,0.9,1,0.9,0.8,0.9,0.9,1),4,4)
Sigma
rank(Sigma)
is.positive.definite(Sigma)
Sigma1=make.positive.definite(Sigma)
is.positive.definite(Sigma1)
Muk1=c(10,10,10,10)
datak1=mvrnorm(n=50,Muk1,Sigma1)
Muk2=c(11,11,11,11)
datak2=mvrnorm(n=50,Muk2,Sigma1)
Muk3=c(12,12,12,12)
datak3=mvrnorm(n=50,Muk3,Sigma1)
data=rbind(datak1,datak2,datak3)
data=data.frame(data,nama)
colnames(data)=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
data=as.data.frame(data)

#Plot
library(ggplot2)
ggplot(data, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point()
my_cols = c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(data[,1:4], pch = 19,  cex = 0.7,
      col = my_cols[data$Species],
      lower.panel=NULL)
library(corrplot)
a=cor(data[,1:4])
corrplot(a, method = "color",
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 90)

fviz_nbclust(data[,1:4],FUN=hcut,method="wss")
ab=dist(data[,1:4],method="euclidian")
clusters <- hclust(ab,method="complete")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="average")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="single")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="ward.D")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 





#simulasi data 3 (Tidak tumpang tindih & Low correlated)
library(MASS)
library(matrixcalc)
library(corpcor)
Sigma=matrix(c(1,0.1,0.1,0.1,0.1,1,0.1,0.1,0.1,0.1,1,0.1,0.1,0.1,0.1,1),4,4)
Sigma
rank(Sigma)
is.positive.definite(Sigma)
Sigma1=make.positive.definite(Sigma)
is.positive.definite(Sigma1)
Muk1=c(10,13,22,2)
datak1=mvrnorm(n=50,Muk1,Sigma1)
Muk2=c(6,7,22,18)
datak2=mvrnorm(n=50,Muk2,Sigma1)
Muk3=c(15,4,22,9)
datak3=mvrnorm(n=50,Muk3,Sigma1)
data=rbind(datak1,datak2,datak3)
data=data.frame(data,nama)
colnames(data)=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
data=as.data.frame(data)

#Plot
library(ggplot2)
ggplot(data, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point()
my_cols = c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(data[,1:4], pch = 19,  cex = 0.7,
      col = my_cols[data$Species],
      lower.panel=NULL)
library(corrplot)
a=cor(data[,1:4])
corrplot(a, method = "color",
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 90)
ab=dist(data[,1:4],method="euclidian")

fviz_nbclust(data[,1:4],FUN=hcut,method="wss")
clusters <- hclust(ab,method="complete")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="average")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="single")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="ward.D")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 





#simulasi data 4 (Tumpang Tindih & Low Correlated)
library(MASS)
library(matrixcalc)
library(corpcor)
Sigma=matrix(c(1,0.1,0.1,0.1,0.1,1,0.1,0.1,0.1,0.1,1,0.1,0.1,0.1,0.1,1),4,4)
Sigma
rank(Sigma)
is.positive.definite(Sigma)
Sigma1=make.positive.definite(Sigma)
is.positive.definite(Sigma1)
Muk1=c(10,13,11,10)
datak1=mvrnorm(n=50,Muk1,Sigma1)
Muk2=c(11,11,12,10)
datak2=mvrnorm(n=50,Muk2,Sigma1)
Muk3=c(11,12,14,11)
datak3=mvrnorm(n=50,Muk3,Sigma1)
data=rbind(datak1,datak2,datak3)
data=data.frame(data,nama)
colnames(data)=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")
data=as.data.frame(data)

#Plot
library(ggplot2)
ggplot(data, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point()
my_cols = c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(data[,1:4], pch = 19,  cex = 0.7,
      col = my_cols[data$Species],
      lower.panel=NULL)
library(corrplot)
a=cor(data[,1:4])
corrplot(a, method = "color",
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 90)

fviz_nbclust(data[,1:4],FUN=hcut,method="wss")
ab=dist(data[,1:4],method="euclidian")
clusters <- hclust(ab,method="complete")
plot(clusters)
clust <- cutree(clusters, k = 9)
rect.hclust(clusters, k = 9, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="average")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="single")
plot(clusters)
clust <- cutree(clusters, k = 9)
rect.hclust(clusters, k = 9, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

clusters <- hclust(ab,method="ward.D")
plot(clusters)
clust <- cutree(clusters, k = 3)
rect.hclust(clusters, k = 3, border = 2:10)
library(factoextra)
fviz_cluster(list(data = data[,1:4], cluster = clust)) 

