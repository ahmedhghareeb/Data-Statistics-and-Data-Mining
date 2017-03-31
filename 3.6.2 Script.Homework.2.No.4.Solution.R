#See:
# https://www.r-bloggers.com/performing-principal-components-regression-pcr-in-r/
df <- as.data.frame(iris)
require(pls)
set.seed (1000)
write.csv(df,"iris.csv")

pcr_model <- pcr(Sepal.Length~., data = iris, scale = TRUE, validation = "CV")
  
summary(pcr_model)

# Plot the root mean squared error
validationplot(pcr_model)
# Plot the R2
validationplot(pcr_model, val.type = "R2")
# plot predicted vs actual for 5 components
predplot(pcr_model)

#Now you can try to use PCR on a traning-test set and evaluate its performance using, for example, using only 3 components.

# Train-test split
train <- iris[1:120,]
y_test <- iris[120:150, 1]
test <- iris[120:150, 2:5]

pcr_model <- pcr(Sepal.Length~., data = train,scale =TRUE, validation = "CV")

pcr_pred <- predict(pcr_model, test, ncomp = 4)
mean((pcr_pred - y_test)^2)
## [1] 0.213731




library(rattle)
rattle()
