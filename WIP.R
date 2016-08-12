
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

