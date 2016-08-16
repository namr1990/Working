
setwd("~/Competitions/DS")
getwd()

library(dplyr)

#train = read_csv("train.csv")
head(train)

sam = train

colnames(sam)[2] = "birth_year"
colnames(sam)[3] = "permit_year"
colnames(sam)[4] = "mark"
colnames(sam)[5] = "tax_then"

#feature engineering
head(sam)

f = sam[c(3,34)]

sum(is.na(f[c(1)]))

f$bucket = ntile(f$permit_year,10)
roll = aggregate(prime_tot_ttc ~ bucket , data = f,FUN = mean)
plot(roll$bucket,roll$prime_tot_ttc)

--------------------------------------------------------


#train1

setwd("~/Competitions/Fuel/CAX_Train_1")
getwd()

temp = list.files()
train1 = bind_rows(lapply(temp, read.csv))

for (i in 0:7)
{
  assign(paste0("train1",i), subset.data.frame(train1,train1$PH == i))
}

setwd("~/Competitions/Fuel/Test")
getwd()

test = read.csv("CAX_Test.csv")
head(test)

View(test$FF)

mean(train17$FF)
sd(train17$FF)

test1 = test

for (i in 0:7)
{
  assign(paste0("test1",i), subset.data.frame(test1,test1$PH == i))
}

library(rpart)

install.packages("rpart")
fit = rpart(FF~ . , data = train11,method="anova" , control=rpart.control(minsplit=2, cp=0.001)) 
