df <- mtcars
na.omit(df)

library(rattle)
rattle()

write.csv(df,"df.csv")

# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2016-11-30 21:59:27 x86_64-w64-mingw32 

# Rattle version 4.1.0 user 'khallinan1'

# This log file captures all Rattle interactions as R commands. 

# Export this log to a file using the Export button or the Tools 
# menu to save a log of all your activity. This facilitates repeatability. For example, exporting 
# to a file called 'myrf01.R' will allow you to type in the R Console 
# the command source('myrf01.R') and so repeat all actions automatically. 
# Generally, you will want to edit the file to suit your needs. You can also directly 
# edit this current log in place to record additional information before exporting. 

# Saving and loading projects also retains this log.

# We begin by loading the required libraries.

library(rattle)   # To access the weather dataset and utility commands.
library(magrittr) # For the %>% and %<>% operators.

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2016-11-30 21:59:34 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///C:/Users/khallinan1/Google Drive/Data Mining/Training Currucilum Clean/6. Some Simple Data Manipulation Tools/df.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-11-30 21:59:34 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 32 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.69*crs$nobs) # 22 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.12*crs$nobs) # 4 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 6 observations

# The following variable selections have been noted.

crs$input <- c("X", "mpg", "cyl", "disp",
               "hp", "drat", "wt", "qsec",
               "vs", "am", "gear")

crs$numeric <- c("mpg", "cyl", "disp", "hp",
                 "drat", "wt", "qsec", "vs",
                 "am", "gear")

crs$categoric <- "X"

crs$target  <- "carb"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-11-30 22:01:11 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 32 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.69*crs$nobs) # 22 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.12*crs$nobs) # 4 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 6 observations



# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 32 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.69*crs$nobs) # 22 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.12*crs$nobs) # 4 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 6 observations

# The following variable selections have been noted.

crs$input <- c("cyl", "disp", "hp", "drat",
               "wt", "qsec", "vs", "am",
               "gear", "carb")

crs$numeric <- c("cyl", "disp", "hp", "drat",
                 "wt", "qsec", "vs", "am",
                 "gear", "carb")

crs$categoric <- NULL

crs$target  <- "mpg"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- "X"
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-11-30 22:01:21 x86_64-w64-mingw32 

# Random Forest 

# The 'randomForest' package provides the 'randomForest' function.

library(randomForest, quietly=TRUE)

# Build the Random Forest model.

set.seed(crv$seed)
crs$rf <- randomForest::randomForest(mpg ~ .,
                                     data=crs$dataset[crs$sample,c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=3,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

crs$rf <- randomForest::randomForest(mpg ~ .,
                                     data=crs$dataset[1:20,c(crs$input, crs$target)], 
                                     ntree=500,
                                     mtry=3,
                                     importance=TRUE,
                                     na.action=randomForest::na.roughfix,
                                     replace=FALSE)

# Generate textual output of 'Random Forest' model.

crs$rf


model.rf <- crs$rf

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 0.05 secs

#============================================================
# Rattle timestamp: 2016-11-30 22:01:41 x86_64-w64-mingw32 

# Evaluate model performance. 

# RF: Generate a Predicted v Observed plot for rf model on df.csv [validate].

crs$pr <- predict(model.rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

crs$pr <- predict(model.rf, newdata=df2)

predict.future <- crs$pr

plot(df$mpg[21:32],predict.future)


# Obtain the observed output for the dataset.

obs <- subset(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]), select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(mpg=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(jitter(fitpoints[[1]]), fitpoints[[2]], asp=1, xlab="mpg (Jittered)", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Random Forest Model
 df.csv [validate]",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()