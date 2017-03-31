# Loading Data from R-Studio Open Datasets ------------------------------------------------------------
data()   # See what data is available

# Load dataset available in R
data("ChickWeight")
a<- 2





# Create new dataset fields based upon logical evaluations -----------------------------------------------

#  (create new field "timecat" or any other name) based upon logic
ChickWeight$timecat <- ifelse(ChickWeight$Time > 10, c("older"), c("younger")) 

# another example: create 3 time categories  
attach(ChickWeight) 
ChickWeight$timecat [ChickWeight$Time  > 14] <- "Elder"
ChickWeight$timecat [ChickWeight$Time  > 8 & ChickWeight$Time  <= 14] <- "Middle Aged"
ChickWeight$timecat [ChickWeight$Time  <= 8] <- "Young"
detach(ChickWeight) 



# Sorting -----------------------------------------------------------------

# sorting examples using the mtcars dataset
data(mtcars)

# sort by mpg
newdata = mtcars[order(-mtcars$mpg),] 
# sort by mpg and cyl
newdata <- mtcars[order(mtcars$mpg, mtcars$cyl),]
#sort by mpg (ascending) and cyl (descending)
newdata <- mtcars[order(mtcars$mpg, -mtcars$cyl),] 



# Merging dataframes -----------------------------------------------------------------

new_car = mtcars[1,]
row.names(new_car) <- "New_car"

mtcars_new = rbind(mtcars,new_car)



# Aggregating -------------------------------------------------------------

attach(mtcars)
aggdata <- aggregate(mtcars,
                     by = list(cyl),
                     FUN = mean,
                     na.rm = TRUE)
print(aggdata)



# Subsetting Example 1 --------------------------------------------------------------

data_big_gas_guzzling_cars <- subset(mtcars,mpg<20 & cyl>5)

mtcars[1,4]= NA

# eliminate rows with NAs
mtcars_no_NA <- na.omit(mtcars)
# eliminate factors

myvars <- names(mtcars) %in% c("mpg","cyl")

mtcars_mpg_cyl <- 
  mtcars[myvars]

mtcars_excluding_mpg_cyl <- mtcars[!myvars]



# Subsetting Example 2 --------------------------------------------------------------

# Now let's subset only thos with 9 months of energy data or more
# First load in the data set in this folder through the "Import Dataset" 
# Button in the Global Environment Window (upper right hand corner)
house_plus_energy_complete_w_9_months_of_energy_data_or_more <- 
  house_plus_energy_data[house_plus_energy_data$num_mnts_energy>8,]



