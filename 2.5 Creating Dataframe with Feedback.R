

df <- data.house

N <- nrow(df)

num.feedback.min <- 10



df1 <-  df[(num.feedback.min+1):N,]
k = 1
for (i in (num.feedback.min+1):N) {
  df1$T.floor.1.t.min.1[k] <- df$T.floor.1[i-1]
  df1$T.floor.1.t.min.2[k] <- df$T.floor.1[i-2]

  df1$T.floor.1.t.min.10[k] <- df$T.floor.1[i-num.feedback.min]
  
  df1$T.floor.2.t.min.1[k] <- df$T.Level.2[i-1]
  df1$T.floor.2.t.min.2[k] <- df$T.Level.2[i-2]

  df1$T.floor.2.t.min.10[k] <- df$T.Level.2[i-num.feedback.min]
  
  
  
  k <- k+1
}

