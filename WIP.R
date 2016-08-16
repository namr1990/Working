
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

head(test11)

sub = rbind(data.frame(id = test10$id , FF = 4132) , data.frame(id = test11$id , FF = 37),
            data.frame(id = test12$id , FF = 1343) , data.frame(id = test13$id , FF = 6855),
            data.frame(id = test14$id , FF = 7374) , data.frame(id = test15$id , FF = 5018),
            data.frame(id = test16$id , FF = 2968) , data.frame(id = test17$id , FF = 1400))

View(sub)

write.csv(sub,"sub.csv",row.names = FALSE)

    
