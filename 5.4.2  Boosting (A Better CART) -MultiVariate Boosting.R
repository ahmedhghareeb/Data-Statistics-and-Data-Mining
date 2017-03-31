# See https://cran.r-project.org/web/packages/mvtboost/vignettes/mvtboost_vignette.html

library(mvtboost)
data("mpg",package="ggplot2")
Y <- mpg[,c("cty","hwy")]      # use both city and highway mileage as dvs
Ys <- scale(Y)                 # recommended that outcomes are on same scale
/ char.ids <- unlist(lapply(X,is.character))
X[,char.ids] <- lapply(X[,char.ids],as.factor)

out <- mvtb(Y=Ys,X=X,          # data
            n.trees=1000,          # number of trees
            shrinkage=.01,         # shrinkage or learning rate
            interaction.depth=3)   # tree or interaction depth

out2 <- mvtb(Y=Ys,X=X,
             n.trees=1000, 
             shrinkage=.01,
             interaction.depth=3,
             
             bag.frac=.5,          # fit each tree to a sub sample of this fraction
             trainfrac=.5,         # only fit the model to this fraction of the data set
             cv.folds=3,           # number of cross-validation folds
             mc.cores=1,           # run the cross-validation in parallel
             seednum=103)          # set the seed number for reproducibility
out2$best.trees


summary(out,covex=TRUE)

summary(out2,covex=TRUE)
