
library(gbm)

boost <- gbm(Current  ~. , data=df.train, 
             distribution = 'gaussian', 
             n.trees = 5000, 
             interaction.depth = 4)

summary(boost)
par(mfrow=c(1,2))
plot(boost, i='Displ')
plot(boost, i='Cond')

boost.pred <- predict (boost, df.validate, n.trees=5000)
mean((boost.pred - df.validate$Current )^2)

library(caret)
ctr <- trainControl(method = "cv", number = 10)

boost.caret <- train(Current ~., df.train,
                     method='bstTree',
                     preProc=c('center','scale'),
                     trControl=ctr)
boost.caret

plot(boost.caret)

boost.caret.pred <- predict(boost.caret, df.validate)
mean((boost.caret.pred - df.validate$Current )^2)


# plot(rr$`DLT Actual`)
# points(rr$`pre DLT`, col = "red", pch=4)


