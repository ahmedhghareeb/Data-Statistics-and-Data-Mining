# Create Dataframe through Script ----------------------------------------------


d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)

mydata <- data.frame(d,e,f)

names(mydata) <- c("ID","Color","Passed")

mydata$Color

g <- cbind(d,e) # combine objects as columns 
h <- rbind(d, e ) # combine objects as rows 


# Accessing & Changing Data Elements in Dataframe -------------------------------------------------

mydata[1,1:3] # columns 1 dataframe
mydata[c("ID","Color")] #columns ID and Color from dataframe

length(mydata) # number of obervations

names(mydata) # names

newobject <- edit(mydata) # edit copy and save a

fix(mydata) # edit in place 


# Creating a List ---------------------------------------------------------
w <- list(name="Fred",mydata, age=5.3)      



rm(g) # delete g
rm(h) # delete h



# Enter data using editor -------------------------------------------------
mydata <- data.frame(age=numeric(0), gender=character(0), weight=numeric(0))
mydata <- edit(mydata)


# Saving and Loading Data -------------------------------------------------

write.csv(mydata,"mydata.csv")

mydata_new <- read.csv("mydata.csv")
