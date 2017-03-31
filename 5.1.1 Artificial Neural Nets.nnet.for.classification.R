 library(nnet)




# Neural Network Classification Applied to Combined Rich Data  -------------------------------------------
# import dataset "Compressor.Failure.Data.Anonymized.csv"

data <- Failure_Data_Anonymized

# create new dataset without missing data 
data.na.omit <- na.omit(data)
myvars <- names(data.na.omit) %in% c("Date.And.Time"    x.col(true)
  cres <- max.col(pred)
  table(true, cres)
}

test.cl(targets_random, classification.results$fitted.values)
