

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

## Preprocess using caret 

limitedTrain = data[c(2:9)]
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)

km <- kmeans(normTrain, centers=14)

## Generate classification for train and test sets
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
table(clusterTrain)

check = subset(limitedTrain, clusterTrain == 2)
View(check)

check1 = cbind(limitedTrain,clusterTrain)
View(check1)

qwe = check1 %>% group_by(clusterTrain) %>% summarise(A_mean = mean(A),
                                                      B_mean = mean(B),
                                                      C_mean = mean(C),
                                                      D_mean = mean(D),
                                                      E_mean = mean(E),
                                                      F_mean = mean(F),
                                                      G_mean = mean(G),
                                                      R_mean = mean(R_Square), count = n())
                                                    
qwe = check1 %>% group_by(clusterTrain) %>% summarise(
                                                      R_mean = mean(R_Square), count = n())

asd = check1[c(9)]
asd1 = data.frame(Asset = 1:nrow(asd), asd)
asd2 = data.frame(Asset = paste0("X",asd1$Asset), Cluster = asd1$clusterTrain)
write.csv(asd2,"asd2.txt",row.names = FALSE)
