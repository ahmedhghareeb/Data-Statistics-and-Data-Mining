library(randomForest)
df <- Typical_Weather_File_Dayton

# subset data
df1 <- subset(df,df$is_Daylight==1)

#var.importance.orderedvar.importance.orderedvar.importance.orderedvar.importance.orderedvar.importance.orderedvar.importance.orderedvar.importance.orderedvar.importance.orderedvar.importance.orderedvar.importance.ordered

model.rf <- randomForest(GHI_W_per_sq_m ~ . - is_Daylight - Date..MM.DD.YYYY. - Time..HH.MM., ntree=400, mtry=3,data = df1)
model.rf

# How Many Trees Needed
plot(randomForest(GHI_W_per_sq_m ~ . - is_Daylight - Date..MM.DD.YYYY. - Time..HH.MM., df1, keep.forest=FALSE, ntree=500), log="y")

# Variable Importance 

var.importance <- importance(model.rf)
var.importance <- as.data.frame(var.importance)
var.importance$factors <- rownames(var.importance)
attach(var.importance)
var.importance.ordered <- var.importance[order(IncNodePurity),] 
detach(var.importance)
var.importance.ordered


varImpPlot(model.rf)

# randomForest(x, y=NULL, xtest=NULL, ytest=NULL, ntree=500,
#              mtry=if (!is.null(y) && !is.factor(y))
#                max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
#              replace=TRUE, classwt=NULL, cutoff, strata,
#              sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
#              nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
#              maxnodes = NULL,
#              importance=FALSE, localImp=FALSE, nPerm=1,
#              proximity, oob.prox=proximity,
#              norm.votes=TRUE, do.trace=FALSE,
#              keep.forest=!is.null(y) && is.null(xtest), corr.bias=FALSE,
#              keep.inbag=FALSE, ...)
