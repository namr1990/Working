
#function for EDA (bivariate)

#create data frame
a = c(23,45,12,56,34,78,98,65,43,32,65,85,52,54,65)
b = c(2,3,4,3,5,6,8,2,9,7,6,5,4,2,3)
d = c('a','w','w','a','w','a','w','s','w','a','s','w','s','a','s')

c = data.frame(feature = a, dependent = b)

q = data.frame(perc = seq(0,1,0.1), bucket_value = quantile(c[,1],probs  = seq(0,1,0.1)))


for ( i in 1: nrow(c))
{
  for (j in 1 : nrow(q))
  {
     if(q[j,2] <= c[i,1] < q[j+1,2] ) q[j,1] else 0
    
  }
}
  

for ( i in 1: nrow(c))
{
  for (j in 1 : nrow(q))
  {
    if(c[i,1] == q[j+1,2])
      c$bucket = q[j,1] else 
      c$bucket =   0
  }
}
