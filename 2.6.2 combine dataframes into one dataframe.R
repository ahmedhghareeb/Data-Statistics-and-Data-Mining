# Combine dataframes df1, df2, df3, etc...
df <- data.frame(A = 1:10, B = 11:20, c = 21:30)
df1 <- as.data.frame(lapply(df, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))


df2 <- na.omit(df1)


df_combine <- rbind(df,df1,df2)  # rbind = row bind
