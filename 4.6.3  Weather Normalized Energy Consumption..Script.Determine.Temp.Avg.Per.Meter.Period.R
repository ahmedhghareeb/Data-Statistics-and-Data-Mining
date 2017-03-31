
# import Energy.Data.UD.Houses.2014.2015.Meter.Dates

min(weat)
nrow(Energy.Data.UD.Houses.2014.2015.Meter.Dates)
# Get temperature average per meter period
for (i in 1:nrow(Energy.Data.UD.Houses.2014.2015.Meter.Dates)){
  weather.actual.hourly.period <- weather.actual.hourly[weather.actual.hourly$YRMODAMI >= Energy.Data.UD.Houses.2014.2015.Meter.Dates$Start.YRMODAMI[i] & 
                                                        weather.actual.hourly$YRMODAMI <  Energy.Data.UD.Houses.2014.2015.Meter.Dates$End.YRMODAMI[i],]
  Energy.Data.UD.Houses.2014.2015.Meter.Dates$Temp.Outdoor.Avg[i] <- mean(weather.actual.hourly.period$Temp.Outside.F,na.rm=TRUE)

}


# Non-Linear Multivariate Regression --------------------------------------
library(minpack.lm)
T.outdoor.F <- Energy.Data.UD.Houses.2014.2015.Meter.Dates$Temp.Outdoor.Avg[4:23]

for (i in 1:nrow(Energy.Data.UD.Houses.2014.2015))
{
  # Create array for gas consumption
  E.gas.month.therm <- rep(0,20)
  E.gas.month.therm <- as.numeric(Energy.Data.UD.Houses.2014.2015[1,30:49]*10)
  df <- cbind(T.outdoor.F=T.outdoor.F,E.gas.month.therm=E.gas.month.therm)
  df
  nlm.multi.UD[i] <- nlsLM(E.gas.month.therm ~  Base +HS*Heaviside(Tbalh - T.outdoor.F )*(Tbalh - T.outdoor.F),data=df, start=list(Base = 2, HS = 1.5, Tbalh =65),algorithm = "port", lower=c(0,  0, 30),upper=c(50, 20, 70),control=nls.control(maxiter = 50, tol = 4e-01, minFactor = 1/1024, printEval = FALSE, warnOnly = FALSE),trace=TRUE)
  
 # nlm.multi.UD[i] <- nlsLM(E.gas.month.therm ~  Base +HS*Heaviside(Tbalh - T.outdoor.F )*(Tbalh - T.outdoor.F), data=df1,start=list(Base = 2, HS = 1.5, Tbalh =65),algorithm = "port", lower=c(0,  0, 30),upper=c(50, 20, 70),control=nls.control(maxiter = 50, tol = 4e-01, minFactor = 1/1024, printEval = FALSE, warnOnly = FALSE),trace=TRUE)
  
  
  summary(nlm.multi.hallinan)
}

nlm.multi.UD <- nlsLM(E.gas.month.therm ~  Base +HS*Heaviside(Tbalh - T.outdoor.F )*(Tbalh - T.outdoor.F) +OS * (Occupancy - 2) + ES * E.ele.month.kWh, data=df1,start=list(Base = 2, HS = 1.5, Tbalh =65, OS = 60,ES = -0.1),algorithm = "port", lower=c(0,  0, 30, 0, -1),upper=c(50, 20, 70, 250, 0),control=nls.control(maxiter = 50, tol = 4e-01, minFactor = 1/1024, printEval = FALSE, warnOnly = FALSE),trace=TRUE)
summary(nlm.multi.hallinan)

# Calc R2
sum(resid(nlm.multi.hallinan)^2)   # RSS
with(df1, sum((E.gas.month.therm - mean(E.gas.month.therm))^2))   # AdjSS
