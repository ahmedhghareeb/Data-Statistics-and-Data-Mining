n.cases <- 240                 # Number of points.
n.vars <- 4                    # Number of mutually correlated variables.
set.seed(26)                   # Make these results reproducible.
eps  <- rnorm(n.vars, 0, 1/4)  # Make "1/4" smaller to *increase* the correlations.
x    <- matrix(rnorm(n.cases * (n.vars+2)), nrow=n.cases)
beta <- rbind(c(1,rep(0, n.vars)), c(0,rep(1, n.vars)), cbind(rep(0,n.vars), diag(eps)))
y    <- x%*%beta               # The variables.

y1<- as.data.frame(y)
cor(y)                         # Verify their correlations are as intended.
plot(data.frame(y))            # Show the scatterplot matrix.                                                                                                                                                                                                                                                                                                               -16L))

lm.results1 <- lm(V5 ~ .,y1)
summary(lm.results1)
# Remove Highly Correlated  -------------------------------------------

tmp <- cor(y)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
# Above two commands can be replaced with 
# tmp[!lower.tri(tmp)] <- 0
#
y.new <- y[,!apply(tmp,2,function(y) any(y > 0.95))]
head(y.new)

