# Import data.house.csv
f <- data.house

summary(data.house)

# histograms
hist(data.house$T.floor.1)
hist(data.house$T.outside)

install.packages("ggplot2")
#To effectively load the ggplot2 package, execute the following command.

library(ggplot2)
# Take the dataset "chol" to be plotted, pass the "AGE" column from the "cho"l" dataset as values on the x-axis and compute a histogram of this
ggplot(data=data.house, aes(data.house$T.outside)) + geom_histogram() + xlab("T.outside") +  geom_histogram(color="black", fill="purple")

# CDF
# https://r-dir.com/blog/2014/03/cdfs-in-r.html
ggplot(data.house, aes(T.outside)) + stat_ecdf(geom = "step")
