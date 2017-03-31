df <- airquality
df
df1 <-  df[complete.cases(df), ]

df2 <- na.omit(df)
