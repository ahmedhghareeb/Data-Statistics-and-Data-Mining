# Load Emission.PM10.Kanata.Kevin.csv
# Load Weather.Data.Semapa.Kevin
# Get Average Weather Data for Each Emission Period
min(Weather.Data.Semapa.Kevin$Start.YYYYMMDDHHMM)
max(Weather.Data.Semapa.Kevin$Start.YYYYMMDDHHMM)
min(Emission.PM10.Kanata.Kevin$Start.YYYYMMDDHHmm)
max(Emission.PM10.Kanata.Kevin$Start.YYYYMMDDHHmm)

# min(Weather.Data.Semapa.Kevin$Start.YYYYMMDDHHMM)
# [1] 2.00901e+11
# > max(Weather.Data.Semapa.Kevin$Start.YYYYMMDDHHMM)
# [1] 201512312345
# > min(Emission.PM10.Kanata.Kevin$Start.YYYYMMDDHHmm)
# [1] 2.01001e+11
# > max(Emission.PM10.Kanata.Kevin$Start.YYYYMMDDHHmm)
# [1] 201412312330

Weather.Data.Semapa.Kevin <- read.csv("Weather.Data.Semapa.Kevin.csv", header=T, na.strings="")

# Get temperature average per meter period
for (i in 35000:nrow(Emission.PM10.Kanata.Kevin)){
  print(i)
  row.numbers.weather <- which(Weather.Data.Semapa.Kevin$Start.YYYYMMDDHHMM >= Emission.PM10.Kanata.Kevin$Start.YYYYMMDDHHmm[i] & 
          +           Weather.Data.Semapa.Kevin$Start.YYYYMMDDHHMM <  Emission.PM10.Kanata.Kevin$End.YYYYMMDDHHmm[i], arr.ind=TRUE)
  weather.data.points.in.emission.period <- Weather.Data.Semapa.Kevin[row.numbers.weather,]
  head(weather.data.points.in.emission.period)
  Emission.PM10.Kanata.Kevin$Temp.C[i] <- mean(as.numeric(weather.data.points.in.emission.period$Temp.C,na.rm=TRUE))
  Emission.PM10.Kanata.Kevin$Wind.Speed.m_s[i] <- mean(as.numeric(weather.data.points.in.emission.period$Wind.Speed.m_s,na.rm=TRUE))
  Emission.PM10.Kanata.Kevin$Wind.Dir.deg[i] <- mean(as.numeric(weather.data.points.in.emission.period$Wind.Dir.deg,na.rm=TRUE))
  Emission.PM10.Kanata.Kevin$Hum.Ratio.percent[i] <- mean(as.numeric(weather.data.points.in.emission.period$Hum.Ratio.percent,na.rm=TRUE))
  Emission.PM10.Kanata.Kevin$Precip.mm[i] <- mean(as.numeric(weather.data.points.in.emission.period$Precip.mm,na.rm=TRUE))
  Emission.PM10.Kanata.Kevin$RAD.W_m2[i] <- mean(as.numeric(weather.data.points.in.emission.period$RAD.W_m2,na.rm=TRUE))
  #print(Emission.PM10.Kanata.Kevin$Wind.Speed.m_s[i])
}
write.csv(Emission.PM10.Kanata.Kevin,"Emission.PM10.Kanata.Kevin.Merged.csv")

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
