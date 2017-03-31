df <- Stewart.Hall.w.Feedback.5.pt




df1 <- na.omit(df)

df1 <- subset(df1,select = -Date.Time)
df1$Hour.Day <- as.numeric(df1$Hour.Day)
df1$Is.Weekday <- as.numeric(df1$Day.Week)
df1$UGEnrollment <- as.numeric(df1$UGEnrollment)
df1$GradEnrollment <- as.numeric(df1$GradEnrollment)
df1$Is.Students <- as.numeric(df1$Is.Students)


library(rpca)
library(rrcovHD)


pca2 <- PcaHubert(~.,data=df1, k=2 ,alpha = 0.95, Scale = TRUE, trace = TRUE)
plot(pca2)
summary(pca2)

pca <- PcaHubert(df1,k=2)

df2b <- df1

df1$od <- pca2@od
df1$sd <- pca2@sd



plot(df1$od)
plot(df1$sd)
