n.cases <- 240                 # Number of points.
n.vars <- 4                    # Number of mutually correlated variables.
set.seed(26)                   # Make these results reproducible.
eps  <- rnorm(n.vars, 0, 1/4)  # Make "1/4" smaller to *increase* the correlations.
x    <- matrix(rnorm(n.cases * (n.vars+2)), nrow=n.cases)
beta <- rbind(c(1,rep(0, n.vars)), c(0,rep(1, n.vars)), cbind(rep(0,n.vars), diag(eps)))
y    <- x%*%beta               # The variables.


y <- University_of_Dayton_Houses_Data_Inputs_and_Target

library(rattle)
rattle()


# A pre-defined value is used to reset the random seed so that results are repeatab



factors.keep <- c("Area.Floor", "Basement.Area", "Attic.Area", "Window.Area",
                 "Wall.Area", "Attic.Rvalue", "Windows.Rvalue", "Walls.Rvalue",
                 "AC.SEER", "Water.Heater.eff", "Refrig.Energy.Year", "Refrig.EF",
                 "Refrig.Size", "Lights.Num.Fluor", "Water.Heater.On.Summer", "AC.On.Summer",
                 "Furnace.Pilot.On.Summer", "Refrig.On.Summer", "Duct.Open.Area.Basement.in2", "Attic.Penetration.in2",
                 "Occupancy.Number", "Electric.Baseline.Intensity.kW.sf", "ElectricTotal.Intensity.kW.sf", "Gas.Baseline.kBTU.sf",
                 "Gas.Heating.kBTU.sf", "Gas.Total.Intensity.kBTU.sf", "Heating.Degree.Hours", "Cooling.Degree.Hours")

y.numeric <- y[,factors.keep]

y<- y.numeric

cor(y)                         # Verify their correlations are as intended.
plot(data.frame(y))            # Show the scatterplot matrix.                                                                                                                                                                                                                                                                                                               -16L))


# Remove Highly Correlated  -------------------------------------------

tmp <- cor(y)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
# Above two commands can be replaced with 
# tmp[!lower.tri(tmp)] <- 0
#
y.new <- y[,!apply(tmp,2,function(y) any(y > 0.95))]
head(y.new)

