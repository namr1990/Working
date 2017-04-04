

.libPaths("//172.28.130.113/Decision Services Share/Ankur Verma/R Packages")
.libPaths()

library(QuantPsyc)
library(glmnet)
library(dplyr)
library(sqldf)
library(MASS)
library(data.table)
library(plyr)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rpart.utils)
library(partykit)



getwd()
setwd("~/Elasticity/Meat")

seed = 0
set.seed(seed)

train = fread("Meat_Model_30.csv")

train$avg_price_gap = as.character(train$avg_price_gap)
train$avg_price_gap = ifelse(train$avg_price_gap == 'NULL' , -9999999 ,train$avg_price_gap )
train$avg_price_gap = (as.numeric(train$avg_price_gap))

func = function(x) length(x)
train = ddply(train ,"cid", transform , cnt = func(cid) )
train = subset(train , cnt > 5 )


map = unique(train$cid)
map = data.frame(map)
map1 = data.frame(row = 1:nrow(map) ,cid = map$map )
train = merge(train,map1)


train1 = subset(train, row == 1 )

train_1 = subset(train1, cal_week < 11601)
test = subset(train1,cal_week < 11601)

train1 = subset(train1 , volume_per_daystore1 > 0 )
test = subset(test,volume_per_daystore1 > 0 )

train2  = train1

test1 = subset(test,cal_week < 11601)
test2  = test1


trainx = train2[c(3,5,8:55)]			

fita <- rpart(volume_per_daystore1 ~ ., data = trainx , method="anova" , control=rpart.control(minsplit=2, cp=0.005))

plot(fita)
text(fita)

get_node_date <- function(tree = a, node = b){
  rule <- path.rpart(tree, node)
  rule_2 <- sapply(rule[[1]][-1], function(x) strsplit(x, '(?<=[><=])(?=[^><=])|(?<=[^><=])(?=[><=])', perl = TRUE))
  ind <- apply(do.call(cbind, lapply(rule_2, function(x) eval(call(x[2], trainx[,x[1]], as.numeric(x[3]))))), 1, all)
  trainx[ind,]
}

node  = get_node_date(tree = fita , node = 1 )
node = data.frame()


for (i in 2:100)
{
   node_int = get_node_date(tree = fita , node = i )
  
}

as   
a = as.data.frame(path.rpart(fita))
     
     pfit <- as.party(fita)
     pfit$data <- model.frame(fita)
     data4 <- data_party(pfit, 8)
     dim(data4)
     
     fita
     
     
     
 #------------------------------------------------------------------------------------------------------------------#
 
 


.libPaths("//172.28.130.113/Decision Services Share/Ankur Verma/R Packages")
.libPaths()

library(QuantPsyc)
library(glmnet)
library(dplyr)
library(sqldf)
library(MASS)
library(data.table)
library(plyr)

install.packages("rattle")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("RColorBrewer")


library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)


getwd()
setwd("~/Elasticity/Meat")

seed = 0
set.seed(seed)

train = fread("Meat_Model.csv")

train$avg_price_gap = as.character(train$avg_price_gap)
train$avg_price_gap = ifelse(train$avg_price_gap == 'NULL' , -9999999 ,train$avg_price_gap )
train$avg_price_gap = (as.numeric(train$avg_price_gap))

func = function(x) length(x)
train = ddply(train ,"cid", transform , cnt = func(cid) )
train = subset(train , cnt > 30 )


map = unique(train$cid)
map = data.frame(map)
map1 = data.frame(row = 1:nrow(map) ,cid = map$map )
train = merge(train,map1)

train1 = subset(train, row == 1 )

train_1 = subset(train1, cal_week < 11601)
test = subset(train1,cal_week < 11601)

train1 = subset(train1 , volume_per_daystore1 > 0 )
test = subset(test,volume_per_daystore1 > 0 )

train2  = train1

test1 = subset(test,cal_week < 11601)
test2  = test1


trainx = train2[c(3,5,8:20,41)]			



fita <- rpart(volume_per_daystore1 ~  unique_retail_price + weekend_days +	jan_days +	feb_days +	mar_days +	apr_days +	may_days
+ jun_days +	jul_days +	aug_days +	sep_days +	oct_days +	nov_days +	dec_days +significant_snow_ind
, data = trainx , method="anova" , control=rpart.control(minsplit=2, cp=0.01))


rpart.plot(fita)


cv.glmmod <- cv.glmnet(trainx,y =  log(train2$volume_per_daystore1) ,alpha=1)
plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min

fit <- glmnet( trainx ,y =  log(train2$volume_per_daystore1) 
               , family="gaussian", alpha=0.5, lambda=best_lambda)



SST = sum((log(train2$volume_per_daystore1) - mean(log(train2$volume_per_daystore1)))^2)
SSR = sum((log(train2$volume_per_daystore1) - predict(object = fit, trainx))^2)
R_Squared = 1- (SSR/SST)
Pearson_R_Squared = (cor(train2$volume_per_daystore1 , predict(object = fit, trainx)))^2

coef(fit)
coef1 = data.frame(Variable = dimnames(coef(fit))[[1]], Beta = matrix(coef(fit)))
coef1 = subset(coef1 , Beta != 0)

result = predict(object = fit, testx)

check = data.frame(actual = test2$volume_per_daystore1 , predict = exp(result), 
                   error =abs(test2$volume_per_daystore1 - exp(result) )/test2$volume_per_daystore1)
total = nrow(check)
Mape = 100*mean(check$s0.1)

price_points = nrow(data.frame(unique(train2$unique_retail_price)))

Non_OT_Train = nrow(train1)
OT_Train = nrow(train2)
OT_Test = nrow(test2)

final = data.frame(cbind(upc = train1[1,1]  ,coef1 , best_lambda , Mape, price_points , Non_OT_Train,OT_Train,OT_Test , R_Squared , Pearson_R_Squared))


for (i in 2:1094)
{
  
  train1 = subset(train, row == i )
  train_1 = subset(train1, cal_week < 11601)
  test = subset(train1,cal_week < 11601)
  
  train1 = subset(train1 , volume_per_daystore1 > 0 )
  test = subset(test,volume_per_daystore1 > 0 )
  
  train2  = train1
  
  test1 = subset(test,cal_week < 11601)
  test2  = test1
  
  
  trainx <- as.matrix(data.frame(log(train2$unique_retail_price) ,
                                 train2$weekend_days,	train2$jan_days,	train2$feb_days,	train2$mar_days,	train2$apr_days,	train2$may_days,	train2$jun_days,
                                 train2$jul_days,	train2$aug_days,	train2$sep_days,	train2$oct_days,	train2$nov_days,	train2$dec_days,
                                 train2$significant_snow_ind, train2$avg_price_gap,	train2$mothers_day_days,	
                                 train2$thanksgiving,	train2$valentines_day,	train2$ash_wednesday,	train2$superbowl,	train2$passover,	train2$st_patricks_day,	
                                 train2$good_friday,	train2$independence_day,	train2$easter,	train2$new_years_day_and_kwanzaa,	train2$mardi_gras,	train2$halloween,	
                                 train2$pay_day__1st___15th_,	train2$payday_lag__4th___18th_,	train2$ceaser_chavez_day,	train2$hanukkah,	train2$cinco_de_mayo,	
                                 train2$cyber_monday,	train2$three_kings_day,	train2$nfl_flag,	train2$nascar_flag,	
                                 train2$store_college_fbcollege_fb,	train2$store_college_fbcollege_fb_lead ,train2$r_1,
                                 train2$r_2,train2$r_3,train2$r_4,train2$r_5,train2$r_6,train2$r_7,train2$r_8,train2$r_9))
  
  
  testx = as.matrix(data.frame(log(test2$unique_retail_price) ,
                               test2$weekend_days,	test2$jan_days,	test2$feb_days,	test2$mar_days,	test2$apr_days,	test2$may_days,	test2$jun_days,
                               test2$jul_days,	test2$aug_days,	test2$sep_days,	test2$oct_days,	test2$nov_days,	test2$dec_days,
                               test2$significant_snow_ind,test2$avg_price_gap,test2$mothers_day_days,	
                               test2$thanksgiving,	test2$valentines_day,	test2$ash_wednesday,	test2$superbowl,	test2$passover,	test2$st_patricks_day,	
                               test2$good_friday,	test2$independence_day,	test2$easter,	test2$new_years_day_and_kwanzaa,	test2$mardi_gras,	test2$halloween,	
                               test2$pay_day__1st___15th_,	test2$payday_lag__4th___18th_,	test2$ceaser_chavez_day,	test2$hanukkah,	test2$cinco_de_mayo,	
                               test2$cyber_monday,	test2$three_kings_day,	test2$nfl_flag,	test2$nascar_flag,	
                               test2$store_college_fbcollege_fb,	test2$store_college_fbcollege_fb_lead,test2$r_1,
                               test2$r_2,test2$r_3,test2$r_4,test2$r_5,test2$r_6,test2$r_7,test2$r_8,test2$r_9))
  
  
  cv.glmmod <- cv.glmnet(trainx,y =  log(train2$volume_per_daystore1) ,alpha=1)
  plot(cv.glmmod)
  best_lambda <- cv.glmmod$lambda.min
  
  fit <- glmnet( trainx ,y =  log(train2$volume_per_daystore1) 
                 , family="gaussian", alpha=0.5, lambda=best_lambda)
  
  SST = sum((log(train2$volume_per_daystore1) - mean(log(train2$volume_per_daystore1)))^2)
  SSR = sum((log(train2$volume_per_daystore1) - predict(object = fit, trainx))^2)
  R_Squared = 1- (SSR/SST)
  Pearson_R_Squared = (cor(train2$volume_per_daystore1 , predict(object = fit, trainx)))^2
  
  coef(fit)
  coef1 = data.frame(Variable = dimnames(coef(fit))[[1]], Beta = matrix(coef(fit)))
  coef1 = subset(coef1 , Beta != 0)
  
  result = predict(object = fit, testx)
  
  check = data.frame(actual = test2$volume_per_daystore1 , predict = exp(result), 
                     error =abs(test2$volume_per_daystore1 - exp(result) )/test2$volume_per_daystore1)
  total = nrow(check)
  Mape = 100*mean(check$s0.1)
  
  price_points = nrow(data.frame(unique(train2$unique_retail_price)))
  
  Non_OT_Train = nrow(train1)
  OT_Train = nrow(train2)
  OT_Test = nrow(test2)
  
  result = data.frame(cbind(upc = train1[1,1]  ,coef1 ,  best_lambda , Mape, price_points , Non_OT_Train,OT_Train,OT_Test , R_Squared , Pearson_R_Squared))
  final = rbind(final,result)
}

write.csv(final , "meat4.csv" , row.names = FALSE)


