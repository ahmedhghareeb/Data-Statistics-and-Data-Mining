library(corrplot, quietly=TRUE)

# Read Data ---------------------------------------------------------------
iris <- read.csv("C:/Users/khallinan1/Google Drive/Data Mining/Training Currucilum Clean/5. Data Mining in R/5.2 Classification and Regression Trees (CART)/Scripts/iris.csv")


# Step 1 - Find Variable Where Greatest Separation is Possible ------------

plot(iris)

  # What do you see from the plots when looking at predicting
  #  Species.No?
  #  - Greatest separation with Petal.Width or Petal.Length

  # Not surpisingly these correspond with the highest correlation

     cor.results <- cor(iris, use="pairwise", method="pearson")

   # Display the actual correlations.

     print(cor.results)

    #Correlation summary using the 'Pearson' covariance.

    #Note that only correlations between numeric variables are reported.

    #              Sepal.Length Sepal.Width Petal.Length Petal.Width Species.No
    #Sepal.Length    1.0000000  -0.1175698    0.8717538   0.8179411  0.4600392
    #Sepal.Width    -0.1175698   1.0000000   -0.4284401  -0.3661259 -0.6183715
    #Petal.Length    0.8717538  -0.4284401    1.0000000   0.9628654  0.6492418
    #Petal.Width     0.8179411  -0.3661259    0.9628654   1.0000000  0.5803770
    #Species.No      0.4600392  -0.6183715    0.6492418   0.5803770  1.0000000

# Highest correlation of Species.No is to petal length. Let's consider this the top
# rated variable.

iris$Species.No <- as.numeric(iris$Species.No)

plot(iris$Petal.Length,iris$Species.No)

petal.length.min = min(iris$Petal.Length)
petal.length.max = max(iris$Petal.Length)

petal.length.vector <- petal.length.min + (rep(1:101)-1) * (petal.length.max-petal.length.min)/100
petal.length.vector

attach(iris)
iris.order.Petal.Length <- iris[order(Petal.Length),] 
detach(iris)

attach(iris)

cnt.1 = vector(,101)
cnt.2 = vector(,101)
cnt.3 = vector(,101)

for (i in 2:100){
  cnt.1[i] <- 0
  cnt.2[i] <- 0
  cnt.3[i] <- 0
  for (j in 1:nrow(iris.order.Petal.Length)) {
    if (iris$Petal.Length[j] < petal.length.vector[i])
    {
      if (iris$Species.No[j] == 1)
      {
        cnt.1[i] <- cnt.1[i] + 1
      }
      if (iris$Species.No[j] == 2)
      {
        cnt.2[i] <- cnt.2[i] + 1
      }
      if (iris$Species.No[j] == 3)
      {
        cnt.3[i] <- cnt.3[i] + 1
      }
    }
  }

}


cnt.1[i+1] <- cnt.1[i]
cnt.2[i+1] <- cnt.2[i]
cnt.3[i+1]  <- cnt.3[i]


purity.measure = vector(,101) # maximum number of separate values
  
for (i in 2:100){
  purity.measure[i] = cnt.1[i] - cnt.2[] - cnt.3[i]
}

branch.pt.1 <- 1
for (i in 2:100) {
  if (purity.measure[i] == max(purity.measure)){
    if (purity.measure[i] == purity.measure[i-1] )
    {
      branch.pt.1 = i
    }}
}

branch.pt.1

petal.length.vector[branch.pt.1]

iris.child.branch.pt.1 <- subset(iris,iris$Petal.Length > petal.length.vector[branch.pt.1])


# Correlations work for numeric variables only.

cor.results.branch.1 <- cor(iris.child.branch.pt.1, use="pairwise", method="pearson")

# Display the actual correlations.

print(cor.results.branch.1)

plot(iris.child.branch.pt.1$Petal.Width, iris.child.branch.pt.1$Species.No)

petal.width.min = min(iris$Petal.Width)
petal.width.max = max(iris$Petal.Width)

petal.width.vector <- petal.width.min + (rep(1:101)-1) * (petal.width.max-petal.width.min)/100
petal.width.vector

attach(iris.child.branch.pt.1)
iris.order.child.branch.pt.1.Petal.Width <- iris.child.branch.pt.1[order(Petal.Width),] 
detach(iris.child.branch.pt.1)



cnt.1 = vector(,101)
cnt.2 = vector(,101)
cnt.3 = vector(,101)

for (i in 2:100){
  cnt.1[i] <- 0
  cnt.2[i] <- 0
  cnt.3[i] <- 0
  for (j in 1:nrow(iris.child.branch.pt.1)) {
    if (iris.child.branch.pt.1$Petal.Width[j] < petal.width.vector[i])
    {
      if (iris.child.branch.pt.1$Species.No[j] == 1)
      {
        cnt.1[i] <- cnt.1[i] + 1
      }
      if (iris.child.branch.pt.1$Species.No[j] == 2)
      {
        cnt.2[i] <- cnt.2[i] + 1
      }
      if (iris.child.branch.pt.1$Species.No[j] == 3)
      {
        cnt.3[i] <- cnt.3[i] + 1
      }
    }
  }
  
}


cnt.1[i+1] <- cnt.1[i]
cnt.2[i+1] <- cnt.2[i]
cnt.3[i+1]  <- cnt.3[i]


purity.measure.1 = vector(,101) # maximum number of separate values

for (i in 2:100){
  purity.measure.1[i] =  cnt.3[] - cnt.2[i]
}

branch.pt.2 <- 1
for (i in 2:100) {
  if (purity.measure.1[i] == max(purity.measure.1)){
    if (purity.measure.1[i] == purity.measure.1[i-1] )
    {
      branch.pt.2 = i
    }}
}

branch.pt.2

petal.width.vector[branch.pt.2]

iris.child.branch.pt.2 <- subset(iris.child.branch.pt.1,iris.child.branch.pt.1$Petal.Width > petal.width.vector[branch.pt.2])





# Correlations work for numeric variables only.

cor.results.branch.2 <- cor(iris.child.branch.pt.2, use="pairwise", method="pearson")

# Display the actual correlations.

print(cor.results.branch.2)

plot(iris.child.branch.pt.2$Petal.Width, iris.child.branch.pt.2$Species.No)

petal.width.min = min(iris.child.branch.pt.2$Petal.Width)
petal.width.max = max(iris.child.branch.pt.2$Petal.Width)

petal.width.vector <- petal.width.min + (rep(1:101)-1) * (petal.width.max-petal.width.min)/100
petal.width.vector

attach(iris.child.branch.pt.2)
iris.order.child.branch.pt.2.Petal.Width <- iris.child.branch.pt.2[order(Petal.Width),] 
detach(iris.child.branch.pt.2)



cnt.1 = vector(,101)
cnt.2 = vector(,101)
cnt.3 = vector(,101)

for (i in 2:100){
  cnt.1[i] <- 0
  cnt.2[i] <- 0
  cnt.3[i] <- 0
  for (j in 1:nrow(iris.child.branch.pt.2)) {
    if (iris.child.branch.pt.2$Petal.Width[j] < petal.width.vector[i])
    {
      if (iris.child.branch.pt.2$Species.No[j] == 1)
      {
        cnt.1[i] <- cnt.1[i] + 1
      }
      if (iris.child.branch.pt.2$Species.No[j] == 2)
      {
        cnt.2[i] <- cnt.2[i] + 1
      }
      if (iris.child.branch.pt.2$Species.No[j] == 3)
      {
        cnt.3[i] <- cnt.3[i] + 1
      }
    }
  }
  
}


cnt.1[i+1] <- cnt.1[i]
cnt.2[i+1] <- cnt.2[i]
cnt.3[i+1]  <- cnt.3[i]


purity.measure.2 = vector(,101) # maximum number of separate values

for (i in 100:2){
  purity.measure.2[i] =  cnt.2[i] - cnt.3[i]
}

branch.pt.3 <- 1
for (i in 100:2) {
  print(i)
  if (purity.measure.2[i] == max(purity.measure.2)){
    if (purity.measure.2[i] == purity.measure.2[i+1] )
    {
      branch.pt.3 = i
    }}
}

branch.pt.3

petal.width.vector[branch.pt.3]

iris.child.branch.pt.2 <- subset(iris.child.branch.pt.1,iris.child.branch.pt.1$Petal.Width < petal.width.vector[branch.pt.2])






