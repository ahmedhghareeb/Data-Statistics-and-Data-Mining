# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2016-08-15 10:55:10 x86_64-w64-mingw32 

# Rattle version 4.1.0 user 'khallinan1'

# This log file captures all Rattle interactions as R commands. 

#Export this log to a file using the Export button or the Tools 
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
# Rattle timestamp: 2016-08-15 10:55:16 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- All_Data_Combined_with_Field_Only_Factors

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#============================================================
# Rattle timestamp: 2016-08-15 10:55:19 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 180734 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 126513 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 27110 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 27111 observations

# The following variable selections have been noted.

crs$input <- c("Date.And.Time", "Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F",
               "Control.Capacity", "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F",
               "Supply.Air.Temperature.F", "Oldham.Wear")

crs$numeric <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
                 "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F",
                 "Oldham.Wear")

crs$categoric <- "Date.And.Time"

crs$target  <- "Category.Name"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-08-15 10:55:29 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 180734 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 126513 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 27110 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 27111 observations

# The following variable selections have been noted.

crs$input <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
               "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F")

crs$numeric <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
                 "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F")

crs$categoric <- NULL

crs$target  <- "Oldham.Wear"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Date.And.Time", "Category.Name")
crs$weights <- NULL


#============================================================
# Rattle timestamp: 2016-08-15 11:15:41 x86_64-w64-mingw32 

# Remap variables. 

# Transform into a factor.

crs$dataset[["TFC_Oldham.Wear"]] <- as.factor(crs$dataset[["Oldham.Wear"]])

ol <- levels(crs$dataset[["TFC_Oldham.Wear"]])
lol <- length(ol)
nl <- c(sprintf("[%s,%s]", ol[1], ol[1]), sprintf("(%s,%s]", ol[-lol], ol[-1]))
levels(crs$dataset[["TFC_Oldham.Wear"]]) <- nl

#============================================================
# Rattle timestamp: 2016-08-15 11:15:42 x86_64-w64-mingw32 

# Note the user selections. 

# The following variable selections have been noted.

crs$input <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
               "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F",
               "TFC_Oldham.Wear")

crs$numeric <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
                 "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F")

crs$categoric <- "TFC_Oldham.Wear"

crs$target  <- NULL
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Date.And.Time", "Oldham.Wear", "Category.Name")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-08-15 11:16:00 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 180734 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 126513 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 27110 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 27111 observations

# The following variable selections have been noted.

crs$input <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
               "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F")

crs$numeric <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
                 "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F")

crs$categoric <- NULL

crs$target  <- "TFC_Oldham.Wear"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Date.And.Time", "Oldham.Wear", "Category.Name")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-08-15 11:16:18 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 180734 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 126513 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 27110 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 27111 observations

# The following variable selections have been noted.

crs$input <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
               "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F")

crs$numeric <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
                 "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F")

crs$categoric <- NULL

crs$target  <- "Category.Name"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Date.And.Time", "Oldham.Wear", "TFC_Oldham.Wear")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-08-15 11:16:37 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 180734 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 126513 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 27110 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 27111 observations

# The following variable selections have been noted.

crs$input <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
               "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F")

crs$numeric <- c("Ambient.Air.Temperature.F", "Compressor.Temperature.F", "Condenser.Coil.Temperature.F", "Control.Capacity",
                 "Control.Temperature.Setpoint.F", "Evaporator.Coil.Temperature.F", "Return.Air.Temperature.F", "Supply.Air.Temperature.F")

crs$categoric <- NULL

crs$target  <- "TFC_Oldham.Wear"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Date.And.Time", "Oldham.Wear", "Category.Name")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-08-15 11:16:48 x86_64-w64-mingw32 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(TFC_Oldham.Wear) ~ .,
                 data=crs$dataset[crs$train,c(crs$input, crs$target)],
                 kernel="rbfdot",
                 prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 8.39 mins

#============================================================
# Rattle timestamp: 2016-08-15 11:26:05 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

table(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$TFC_Oldham.Wear, crs$pr,
      useNA="ifany",
      dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(crs$dataset[crs$sample, c(crs$input, crs$target)])$TFC_Oldham.Wear, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))