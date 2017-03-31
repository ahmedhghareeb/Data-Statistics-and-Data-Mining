x <- 1:100
x0 <- runif(100, 0.0, 1.0)
x00 <- runif(100,0.0,1.0)
x000 <- runif(100,0.0,1.0)
x1 <- x + x0 *0.1
x2 <- x + x00 *0.1
x3 <- x + x000 *0.1
y <- 100  * runif(100, 0.0, 1.0)^2

f <- 2* (x-2)^ 1.3 + 3*( y)^.5


df1 <- lm(f ~ x + y)
summary(df1)

df <- lm(f ~ x +x1 + x2 + x3 + y)
summary(df)
