# See http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm#dummy

#Dummy Coding
#Level of race	        race.f1 (1 vs. 2)	       race.f2 (1 vs. 3)    race.f3 (1 vs. 4)
# 1 (Hispanic)	               0	                        0	                   0
# 2 (Asian)	                   1	                        0	                   0
# 3 (African American)	       0	                        1	                   0
# 4 (Caucasian)	               0	                       0	                  1

hsb2 <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")

# create factor
hsb2$race.f <- factor(hsb2$race)

#assigning the treatment contrasts to race.f
contrasts(hsb2$race.f) = contr.treatment(4)

summary(lm(write ~ factor(race), data = hsb2))

summary(lm(write ~ race, data = hsb2))

summary(lm(write ~ race.f, data = hsb2))

hsb2$race.hispanic <- ifelse(hsb2$race == 1,1,0)
hsb2$race.asian <-  ifelse(hsb2$race == 2,1,0)
