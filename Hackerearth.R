

setwd("/Users/ankur/Documents/Competitions/Hackearth")
getwd()

library(data.table)
library(flexclust)
library(caret)
library(dplyr)

train = fread("train.csv")
test = fread("test.csv")

train$Y = NULL

tt = rbind(train,test)
tt = data.frame(tt)

rm(data)
rm(data1)

pol = lm(formula = tt[,2] ~ poly(tt$Time,6,raw = TRUE))
X1 =as.data.frame(summary(pol)$coef)
a = summary(pol)$r.squared
data = data.frame(ID = 2,A = X1[1,1],B = X1[2,1],C = X1[3,1],D= X1[4,1],E = X1[5,1],F = X1[6,1],G = X1[7,1],R_Square = a)

summary(pol)

for (i in 3:101)
{
  pol = lm(formula = tt[,i] ~ poly(tt$Time,6,raw = TRUE))
  a = summary(pol)$r.squared
  X1 =as.data.frame(summary(pol)$coef)
  data1 = data.frame(ID = i,A = X1[1,1],B = X1[2,1],C = X1[3,1],D= X1[4,1],E = X1[5,1],F = X1[6,1],G = X1[7,1],R_Square = a)
  data = rbind(data,data1)
}

data = transp

## Preprocess using caret 

limitedTrain = data
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)

km <- kmeans(normTrain, centers=7)

## Generate classification for train and test sets
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
table(clusterTrain)

check = subset(limitedTrain, clusterTrain == 1)
View(check)

check1 = cbind(limitedTrain,clusterTrain)
View(check1)

asd = check1[c(4362)]
asd1 = data.frame(Asset = 1:nrow(asd), asd)
asd2 = data.frame(Asset = paste0("X",asd1$Asset), Cluster = asd1$clusterTrain)
write.csv(asd2,"asd2.txt",row.names = FALSE)

#transposed dataset

transp = t(tt)
transp = data.frame(transp)
transp  = transp[2:101,]

apply(transp,2,var)
pca = prcomp(transp)
plot(pca)
biplot (pca , scale =0)

#correlation

ttc = tt
ttc$Time = NULL
asd = data.frame(cor(ttc))


wer = asd

for (i in 1:100)
  {
  for (j in 1:100)
    {
       if(wer[i,j] < 0.9)
       {
         wer[i,j] = NA
       }
    }
  }



for (i in 1:100)
{
  for (j in 1:100)
  {
    wer[i,j][(!is.na(wer[i,j]))] = j
  }
} 



write.csv(wer,"wer.csv",row.names = FALSE)  

#percentile based slopes


PN = data.frame(TIME =tt$Time,X = tt[,2], PT = ntile(tt[,2],10))

sub = subset(PN, PN$PT == 1)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data = data.frame(ID = 1,M1 =  X1[2,1])

sub = subset(PN, PN$PT == 2)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data1 = data.frame(ID = 1,M2 =  X1[2,1])
data = cbind(data,M2 = data1$M2)

sub = subset(PN, PN$PT == 3)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data1 = data.frame(ID = 1,M3 =  X1[2,1])
data = cbind(data,M3 = data1$M3)

sub = subset(PN, PN$PT == 4)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data1 = data.frame(ID = 1,M4 =  X1[2,1])
data = cbind(data,M4 = data1$M4)

sub = subset(PN, PN$PT == 5)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data1 = data.frame(ID = 1,M5 =  X1[2,1])
data = cbind(data,M5 = data1$M5)

sub = subset(PN, PN$PT == 6)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data1 = data.frame(ID = 1,M6 =  X1[2,1])
data = cbind(data,M6 = data1$M6)

sub = subset(PN, PN$PT == 7)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data1 = data.frame(ID = 1,M7 =  X1[2,1])
data = cbind(data,M7 = data1$M7)

sub = subset(PN, PN$PT == 8)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data1 = data.frame(ID = 1,M8 =  X1[2,1])
data = cbind(data,M8 = data1$M8)

sub = subset(PN, PN$PT == 9)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data1 = data.frame(ID = 1,M9 =  X1[2,1])
data = cbind(data,M9 = data1$M9)

sub = subset(PN, PN$PT == 10)
as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
X1 =as.data.frame(summary(as)$coef)
data1 = data.frame(ID = 1,M10 =  X1[2,1])
data = cbind(data,M10 = data1$M10)
data2 = data

for ( i in 3:101)
{
  
  PN = data.frame(TIME =tt$Time,X = tt[,i], PT = ntile(tt[,i],10))
  
  sub = subset(PN, PN$PT == 1)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data = data.frame(ID = i,M1 =  X1[2,1])
  
  sub = subset(PN, PN$PT == 2)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data1 = data.frame(ID = i,M2 =  X1[2,1])
  data = cbind(data,M2 = data1$M2)
  
  sub = subset(PN, PN$PT == 3)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data1 = data.frame(ID = i,M3 =  X1[2,1])
  data = cbind(data,M3 = data1$M3)
  
  sub = subset(PN, PN$PT == 4)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data1 = data.frame(ID = i,M4 =  X1[2,1])
  data = cbind(data,M4 = data1$M4)
  
  sub = subset(PN, PN$PT == 5)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data1 = data.frame(ID = i,M5 =  X1[2,1])
  data = cbind(data,M5 = data1$M5)
  
  sub = subset(PN, PN$PT == 6)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data1 = data.frame(ID = i,M6 =  X1[2,1])
  data = cbind(data,M6 = data1$M6)
  
  sub = subset(PN, PN$PT == 7)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data1 = data.frame(ID = i,M7 =  X1[2,1])
  data = cbind(data,M7 = data1$M7)
  
  sub = subset(PN, PN$PT == 8)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data1 = data.frame(ID = i,M8 =  X1[2,1])
  data = cbind(data,M8 = data1$M8)
  
  sub = subset(PN, PN$PT == 9)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data1 = data.frame(ID = i,M9 =  X1[2,1])
  data = cbind(data,M9 = data1$M9)
  
  sub = subset(PN, PN$PT == 10)
  as = lm(formula = sub$X ~ poly(sub$TIME,1,raw = TRUE))
  X1 =as.data.frame(summary(as)$coef)
  data1 = data.frame(ID = i,M10 =  X1[2,1])
  data = cbind(data,M10 = data1$M10)
  
  data2 = rbind(data2,data)
}

normTrain = data2[-c(1)]
km <- kmeans(normTrain, centers=15)

## Generate classification for train and test sets
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
table(clusterTrain)

check = subset(limitedTrain, clusterTrain == 1)
View(check)

check1 = cbind(limitedTrain,clusterTrain)
View(check1)

asd = check1[c(4362)]
asd1 = data.frame(Asset = 1:nrow(asd), asd)
asd2 = data.frame(Asset = paste0("X",asd1$Asset), Cluster = asd1$clusterTrain)
write.csv(asd2,"asd2.txt",row.names = FALSE)
