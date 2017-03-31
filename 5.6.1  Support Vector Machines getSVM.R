library(e1071)
df<- Loss.of.Charge.Upper.Right.Hand.Corner.Low.Freq.More.High.Freq.Feedback
df1 <- df

df$Age <- as.numeric(df$Age)

df.y <- subset(df,select=c(Charge.Amount))
df.x <- subset(df, select=-c(Charge.Amount))


model1.svm <- svm(y = df.y,x = df.x)


predictedY <- model.svm$fitted




df$X <- 1:nrow(df)
plot(df$X[400:1222],df$Charge.Amount[400:1222])
points(df$X[400:1222], predictedY[400:1222], col = "red", pch=4)


plot(df$X[250000:300000],df$Charge.Amount[250000:300000])
points(df$X[250000:300000], predictedY[250000:300000], col = "red", pch=4)

plot(df$X[250000:260000],df$Charge.Amount[250000:260000])
points(df$X[250000:260000], predictedY[250000:260000], col = "red", pch=4)


plot(df$X[260200:269000],df$Charge.Amount[260200:269000])
points(df$X[260200:269000], predictedY[260200:269000], col = "red", pch=4)



plot(df$Charge.Amount,predictedY)

# calculate R-squared


# Test on slow leak data
df.new <- Log.11.Slow.Leak.13600.BTU.77.77.Sampled.at.8.and.500.periods
df.new$X <- 1:nrow(df.new)

df.new.y <- subset(df.new,select=c(Charge.Amount))
df.new.x <- subset(df.new, select=-c(Charge.Amount))


YPredicted.New <- predict(model.svm,df.new.x)
Yobs.New <- df.new.y[1:294,1]
Rsquared.New <- getRsquared(Yobs.New,YPredicted.New)
Rsquared.New

plot(df.new$X[1:294],df.new$Charge.Amount[1:294])
points(df.new$X[1:294], YPredicted.New, col = "red", pch=4)


# calculate R-squared
getRsquared <- function(Y.obs, Y.pred){

Y.obs.mean <- mean(Y.obs)
SS.Tot <- sum( (Y.obs - Y.obs.mean)^2)
SS.Res <- sum( (Y.obs - Y.pred)^2)
R2 <- 1 - SS.Res/SS.Tot
R2

return(R2)
}
