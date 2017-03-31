# Neural Network Forecasting ----------------------------------------------
library(neuralnet)


# Example 1 ------------------------------------------------------------------

set.seed(500)
library(MASS)

df2 <- Boston


#There is no missing data, good. We proceed by randomly splitting the data 
#into a train and a test set, then we fit a linear regression model and test 
#it on the test set. Note that I am using the gml() function instead of the lm() 
#this will become useful later when cross validating the linear model.


index <- sample(1:nrow(data),round(0.75*nrow(data)))  # round refers to the number of training samples
max(index)
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

#As a first step, we are going to address data preprocessing.
#It is good practice to normalize your data before training a neural network. 
#I cannot emphasize enough how important this step is: depending on your dataset, 
#avoiding normalization may lead to useless results or to a very difficult 
#training process (most of the times the algorithm will not converge before 
#the number of maximum iterations allowed). You can choose different methods 
#to scale the data (z-normalization, min-max scale, etc.). I chose to use the 
#min-max method and scale the data in the interval [0,1]. Usually scaling in 
#the intervals [0,1] or [-1,1] tends to give better results.
#We therefore scale and split the data before moving on:


maxs <- apply(data, 2, max)   # get maximum of all cols (2 refers to columns)
mins <- apply(data, 2, min)   # get minimum of all cols

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

#There is no fixed rule as to how many layers and neurons to use although there are several 
# more or less accepted rules of thumb. Usually, if at all necessary, one hidden layer is enough 
# for a vast numbers of applications. As far as the number of neurons is concerned, it should be 
# between the input layer size and the output layer size, usually 2/3 of the input size. At least in 
# my brief experience testing again and again is the best solution since there is no guarantee that 
# any of these rules will fit your model best.
# Since this is a toy example, we are going to use 2 hidden layers with this configuration: 13:5:3:1. 
# The input layer has 13 inputs, the two hidden layers have 5 and 3 neurons and the output layer has, 
# of course, a single output since we are doing regression.
# Let's fit the net:
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,5,5,5,5,5,5,5),linear.output=T)  # 5 columns in first hidden layer, 5 in second hidden layer, 5 in th

# A couple of notes:
#  For some reason the formula y~. is not accepted in the neuralnet()function. You need to first 
# write the formula and then pass it as an argument in the fitting function.
#  .	The hidden argument accepts a vector with the number of neurons for each hidden layer, 
# while the argument linear.output is used to specify whether we want to do regression linear.
# output=TRUE or classification linear.output=FALSE

#plot(nn)


pr.nn <- compute(nn,test_[,1:13]) 

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv) # unnormalizing
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)


# we then compare the two MSEs
print(paste(MSE.lm,MSE.nn))



# Example 2 - Energy Retailer ---------------------------------------------
# Import data_IGS_with_inputs_and_target.csv through Important Dataset
df1 <- df

df <- na.omit(df,)

index <- sample(1:nrow(data),round(0.75*nrow(data)))  # round refers to the number of training samples
max(index)
train <- data[index,]
test <- data[-index,]

# Linear Model for Comparison
lm.fit <- glm(MeanEnergy~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$MeanEnergy)^2)/nrow(test)


maxs <- apply(data, 2, max)   # get maximum of all cols
mins <- apply(data, 2, min)   # get minimum of all cols

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]


library(neuralnet)
n <- names(train_)
f <- as.formula(paste("MeanEnergy ~", paste(n[!n %in% "MeanEnergy"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(6,6,6),linear.output=FALSE)

plot(nn)


pr.nn <- compute(nn,test_[,1:7])  # 7 input variables.

pr.nn_ <- pr.nn$net.result*(max(data$MeanEnergy)-min(data$MeanEnergy))+min(data$MeanEnergy)
test.r <- (test_$MeanEnergy)*(max(data$MeanEnergy)-min(data$MeanEnergy))+min(data$MeanEnergy)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
# we then compare the two MSEs
print(paste(MSE.lm,MSE.nn))


# Energy Retailer Data Neural Net TRIAL 2 -----------------------------------------
data$index <- sample(1:nrow(data),round(0.75*nrow(data)))  # round refers to the number of training samples
max(index)
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(MeanEnergy~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$MeanEnergy)^2)/nrow(test)


maxs <- apply(data, 2, max)   # get maximum of all cols
mins <- apply(data, 2, min)   # get minimum of all cols

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]


library(neuralnet)
n <- names(train_)
f <- as.formula(paste("MeanEnergy ~", paste(n[!n %in% "MeanEnergy"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(6,6,6),linear.output=FALSE)

plot(nn)


pr.nn <- compute(nn,test_[,1:8])  # 8 input variables.

pr.nn_ <- pr.nn$net.result*(max(data$MeanEnergy)-min(data$MeanEnergy))+min(data$MeanEnergy)
test.r <- (test_$MeanEnergy)*(max(data$MeanEnergy)-min(data$MeanEnergy))+min(data$MeanEnergy)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
# we then compare the two MSEs
print(paste(MSE.lm,MSE.nn))

