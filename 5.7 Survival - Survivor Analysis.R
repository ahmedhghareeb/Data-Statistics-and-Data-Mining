# Survival Package --------------------------------------------------------
# Create a Survival Object
# 
# Create a survival object, usually used as a response variable in a model formula. Argument matching is special for this function, see Details below.
# 
# Usage
# 
# Surv(time, time2, event,
#      type=c('right', 'left', 'interval', 'counting', 'interval2', 'mstate'),
#      origin=0)
# is.Surv(x)
# Arguments
# 
# time	- for right censored data, this is the follow up time. For interval data, the first argument is the starting time for the interval.
# event	- The status indicator, normally 0=alive, 1=dead. Other choices are TRUE/FALSE (TRUE = death) or 1/2 (2=death). For interval censored data, the status indicator is 0=right censored, 1=event at time, 2=left censored, 3=interval censored. Although unusual, the event indicator can be omitted, in which case all subjects are assumed to have an event.
# time2	- ending time of the interval for interval censored or counting process data only. Intervals are assumed to be open on the left and closed on the right, (start, end]. For counting process data, event indicates whether an event occurred at the end of the interval.
# type	- character string specifying the type of censoring. Possible values are "right", "left", "counting", "interval", "interval2" or "mstate".
# origin	-for counting process data, the hazard function origin. This option was intended to be used in conjunction with a model containing time dependent strata in order to align the subjects properly when they cross over from one strata to another, but it has rarely proven useful.
# x	-any R object.

hmohiv<-read.table("http://www.ats.ucla.edu/stat/r/examples/asa/hmohiv.csv", sep=",", header = TRUE) 
library(survival)
attach(hmohiv)
mini<-hmohiv[ID<=5,]
mini

attach(mini)
mini.surv <- survfit(Surv(time, censor)~ 1, conf.type="none")
summary(mini.surv)

plot(mini.surv, xlab="Time", ylab="Survival Probability")


detach(mini)
attach(hmohiv)
hmohiv.surv <- survfit( Surv(time, censor)~ 1, conf.type="none")
summary(hmohiv.surv)

plot (hmohiv.surv,  xlab="Time", ylab="Survival Probability" )




library(survival)
rearrest<-read.table("http://www.ats.ucla.edu/stat/examples/alda/rearrest.csv", 
                     sep=",", header=T)
rearrest0 <- subset(rearrest, personal == 0)
rearrest1 <- subset(rearrest, personal == 1)
f14.1.0 <- summary(survfit(Surv(rearrest0$months, abs(rearrest0$censor -1))  ~ 1))
f14.1.1 <- summary(survfit(Surv(rearrest1$months, abs(rearrest1$censor -1 )) ~ 1))
s.hat.0 <- f14.1.0[1][[1]]
time.0 <- f14.1.0[2][[1]]
s.hat.1 <- f14.1.1[1][[1]]
time.1 <- f14.1.1[2][[1]]
s.hat.steps.0 <- stepfun(time.0, c(1, s.hat.0))
s.hat.steps.1 <- stepfun(time.1, c(1, s.hat.1))
plot(s.hat.steps.0, do.points = FALSE, xlim = c(0, 36), ylim = c(0,1),
       ylab = "Estimated Survival", xlab = "Months after release", main = "")
lines(s.hat.steps.1, do.points = FALSE, xlim = c(0,36), lty = 2)
legend("bottomleft", c("Personal = 0", "Personal = 1"), lty = c(1, 2))
plot(s.hat.steps.0, do.points = FALSE, xlim = c(0, 36), ylim = c(0,1),
     ylab = "Estimated Survival", xlab = "Months after release", main = "")
lines(s.hat.steps.1, do.points = FALSE, xlim = c(0,36), lty = 2)
legend("bottomleft", c("Personal = 0", "Personal = 1"), lty = c(1, 2)) 
