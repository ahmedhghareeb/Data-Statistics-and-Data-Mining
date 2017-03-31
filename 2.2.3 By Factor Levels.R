df <- airquality
df

df1 <-  df[complete.cases(df), ]
df1

df2 <- df1[df1$Ozone > 20 & df1$Wind < 7, ]


df2$NEW <- "Bad"

df2$NEW[1:5] = "Good"

df2 <- df2[df2$NEW == "Good",]
