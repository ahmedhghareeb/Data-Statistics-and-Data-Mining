a = 2
b =2 

b <- 2

library(googleVis)
df=data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))

Bar <- gvisBarChart(df)
plot(Bar)