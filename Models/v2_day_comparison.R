################################################################################
##                      Comparison Between Days                               ##
################################################################################
## Comparisons between day 1 and day 2 of dog sightings                       ##
################################################################################
## Created Nov. 18, 2025, by Bronte Slote, last edited May. 22, 2026          ##
################################################################################


##LOAD LIBRARIES##
library(readr) #reading csv files
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(stringr) #manipulating text
library(tidyr)
library(ggeffects)
library(lme4)
library(RVAideMemoire)
library(car)
library(DHARMa)


################################################################################

##LOAD DATA##

#Create summary data frame dropping all day 2
#import dog density .rds
dog_density <- readRDS("Data/density_v2.rds", refhook = NULL)
#remove all day 2
density_no_re <- dog_density %>%
  filter(day == 1)

################################################################################

##MODEL 1 - DOG DENSITY WITHOUT RESIGHTS##

#Most complex model - since intervention
m1_since <- glmer(Sighting.Count ~ since_intervention + subdistrict + Mode.Transport +
                    (1 | polygon) +
                    offset(log(Track.Length)), 
                  family = poisson, data = density_no_re,control=glmerControl(optimizer="bobyqa"))
summary(m1_since)
#check VIF
vif(m1_since) #all good
#drop 1
drop1(m1_since, test = "Chisq") #drop mode.transport

#Updated model 1 since
m1.1_since <- glmer(Sighting.Count ~ since_intervention + subdistrict + 
                    (1 | polygon) +
                    offset(log(Track.Length)), 
                  family = poisson, data = density_no_re,control=glmerControl(optimizer="bobyqa"))
#drop 1 test
drop1(m1.1_since, test = "Chisq") #all significant, keep all

################################################################################

#Check model fit

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1.1_since)

#Check with DHARMa
simulationOutput_m1_since <- simulateResiduals(fittedModel = m1.1_since) #create simulated data

testDispersion(simulationOutput_m1_since)

testOutliers(simulationOutput_m1_since)

testZeroInflation(simulationOutput_m1_since)

testUniformity(simulationOutput_m1_since)

plot(simulationOutput_m1_since)

################################################################################
################################################################################

##MODEL 1 - TOTAL EFFORT##

#most complex
m1_total <- glmer(Sighting.Count ~ effort_humanpop + subdistrict + Mode.Transport +
                    (1 | polygon) +
                    offset(log(Track.Length)), 
                  family = poisson, data = density_no_re,control=glmerControl(optimizer="bobyqa"))
summary(m1_total)
#check VIF
vif(m1_total) #all good
#check drop 1
drop1(m1_total, test = "Chisq") #drop mode.transport

#Updated model 1 total
m1.1_total <- glmer(Sighting.Count ~ effort_humanpop + subdistrict +
                      (1 | polygon) +
                      offset(log(Track.Length)), 
                    family = poisson, data = density_no_re,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.1_total, test = "Chisq") #all significant

################################################################################

#Check model fit

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1.1_total)

#Check with DHARMa
simulationOutput_m1_total <- simulateResiduals(fittedModel = m1.1_total) #create simulated data

testDispersion(simulationOutput_m1_total)

testOutliers(simulationOutput_m1_total)

testZeroInflation(simulationOutput_m1_total)

testUniformity(simulationOutput_m1_total)

plot(simulationOutput_m1_total)

################################################################################
################################################################################

##MODEL 1 - ANNUAL EFFORT##

#most complex model
m1_year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict + Mode.Transport +
                   (1 | polygon) +
                   offset(log(Track.Length)), 
                 family = poisson, data = density_no_re,control=glmerControl(optimizer="bobyqa"))
summary(m1_year)
#check VIF
vif(m1_year) #all good
#check drop 1
drop1(m1_year, test = "Chisq") #drop effort 2y

#updated model
m1.1_year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_1y_humanpop + subdistrict + Mode.Transport +
                      (1 | polygon) +
                      offset(log(Track.Length)), 
                    family = poisson, data = density_no_re,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.1_year, test = "Chisq") #drop effort 1y

#updated model
m1.2_year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + subdistrict + Mode.Transport +
                      (1 | polygon) +
                      offset(log(Track.Length)), 
                    family = poisson, data = density_no_re,control=glmerControl(optimizer="bobyqa"))
#check drop 1
drop1(m1.2_year, test = "Chisq") #drop mode.transport

#updated model
m1.3_year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + subdistrict +
                      (1 | polygon) +
                     offset(log(Track.Length)), 
                   family = poisson, data = density_no_re,control=glmerControl(optimizer="bobyqa"))

#check drop 1
drop1(m1.3_year, test = "Chisq") #all significant

################################################################################

#Check model fit

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1.3_year)

#Check with DHARMa
simulationOutput_m1_year <- simulateResiduals(fittedModel = m1.3_year) #create simulated data

testDispersion(simulationOutput_m1_year)

testOutliers(simulationOutput_m1_year)

testZeroInflation(simulationOutput_m1_year)

testUniformity(simulationOutput_m1_year)

plot(simulationOutput_m1_year)

################################################################################
################################################################################

##COMPARE AIC##

#Dog Density
AIC(m1.1_since, m1.1_total, m1.3_year)#since intervention model has lowest AIC

#reflects similar results as to model with re-sights

################################################################################
