df <- airquality
df

df1 <-  df[complete.cases(df), ]
df1

df2 <- df1[sample(nrow(df1), 0.5*nrow(df1)), ]
