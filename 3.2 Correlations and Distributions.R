# Import data.house.csv

df <- University_of_Dayton_Houses_Data_Inputs_and_Target
df <- as.data.frame(df)


drops <- c("Lights.Watts.bulb","Basement.Rvalue")
df1 <- df[ , !(names(df) %in% drops)]

df1 <- df1[ , -c(1,2)]
# Pearson Correlation -----------------------------------------------------

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.
library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.


cor.results <- cor(df1, use="pairwise", method="pearson")

# Order the correlations by their strength.

cor.ord <- order(cor.results[1,])
cor.cor <- cor.results[cor.ord, cor.ord]

# Display the actual correlations.

print(cor.cor)

# Graphically display the correlations.

corrplot(cor.cor, mar=c(0,0,1,0))


