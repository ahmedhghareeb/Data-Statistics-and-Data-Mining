# input your df here with both input and target variables. Keep
# colinear and autocorrelated variables. No categorical variables. All Numbers. 

df <-   # Whatever your input file is


# https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
#Now you can try to use PCR on a traning-test set and evaluate its performance using, for example, using only 3 components.


# Select Predictors only  ...Eliminate target
df.inputs <- subset(df,select = -c(Gas.Total.Intensity.kBTU.sf))

# Calculate principal components
prin_comp <- prcomp(df.inputs, scale. = T)

# Plot first two principal components
biplot(prin_comp, scale = 0)

# Compute standard deviation of each principal component
std_dev <- prin_comp$sdev

# Compute variance of each component
pr_var <- std_dev^2

# Check variance of first 10 components
pr_var[1:10]

# Calculate percentage of variance explained
prop_varex <- pr_var/sum(pr_var)


#cumulative scree plot of variation
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")

# Now We want to create models to predice target based upon the principal
#  components

# create dataset with all principal components...This should fit best. 
test.data <- predict(prin_comp, newdata = df)
test.data <- as.data.frame(test.data) 

# Create datset with just the first five principle components. 
test.data.5 <- test.data[,1:5]

# Create datset with just the first ten principle components. 
test.data.10 <- test.data[,1:10]

# Add target variable (the predicted variable into these data frames)
test.data$target <- df$Gas.Total.Intensity.kBTU.sf
test.data.5$target <- df$Gas.Total.Intensity.kBTU.sf
test.data.10$target <- df$Gas.Total.Intensity.kBTU.sf


# develop linear models with each of these dataframes to predict target

lm.all <- lm(target ~., test.data)
summary(lm.all)

lm.10 <- lm(target ~., test.data.10)
summary(lm.10)

lm.5 <- lm(target ~., test.data.5)
summary(lm.5)


