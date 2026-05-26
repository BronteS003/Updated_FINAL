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

#Create sightings data frame dropping all day 2
#import sightings .rds
sightings<- readRDS("Data/sightings_v2.rds", refhook = NULL)
#remove all day 2 observations
sightings_no_re <- sightings %>%
  filter(day == 1)

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

##MODEL 2 - STERILIZATION STATUS##

#Remove puppies as they're not relevant to analysis (cannot be sterilized)
sightings_ster <- sightings_no_re %>% 
  filter(Puppy != 1)

#Remove adults with unknown sterilization status 
sightings_ster <- sightings_ster %>% 
  filter(Unknown != 1)

################################################################################

#Most complex model - since intervention
m2_since <- glmer(Neutered ~ since_intervention + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
summary(m2_since)
#check vif
vif(m2_since) #all good
#drop 1 test
drop1(m2_since, test = "Chisq") #drop subdistrict

#Updated model 2 since
m2.1_since <- glmer(Neutered ~ since_intervention + owned + sex +
                    (1 | polygon),
                  family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#drop 1 test
drop1(m2.1_since, test = "Chisq") #drop owned

#Updated model 2 since
m2.2_since <- glmer(Neutered ~ since_intervention + sex +
                    (1 | polygon),
                  family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#drop 1 test
drop1(m2.2_since, test = "Chisq") #both significant

################################################################################

#Check fit

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.2_since)

#Check with DHARMa
simulationOutput_m2_since <- simulateResiduals(fittedModel = m2.2_since) #create simulated data

testDispersion(simulationOutput_m2_since)

testOutliers(simulationOutput_m2_since)

testZeroInflation(simulationOutput_m2_since)

testUniformity(simulationOutput_m2_since)

plot(simulationOutput_m2_since)

################################################################################
################################################################################

##Total effort model

#Most complex model
m2_total <- glmer(Neutered ~ effort_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))

#check VIF
vif(m2_total) #all good

#drop 1 test
drop1(m2_total, test = "Chisq") #drop subdistrict

#Updated model
m2.1_total <- glmer(Neutered ~ effort_humanpop + owned + sex +
                    (1 | polygon),
                  family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))

#drop 1 test
drop1(m2.1_total, test = "Chisq") #drop owned

#Updated model
m2.2_total <- glmer(Neutered ~ effort_humanpop + sex +
                    (1 | polygon),
                  family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#drop 1 test
drop1(m2.2_total, test = "Chisq") #both significant

################################################################################

#Check fit

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.2_total)

#Check with DHARMa
simulationOutput_m2_total <- simulateResiduals(fittedModel = m2.2_total) #create simulated data

testDispersion(simulationOutput_m2_total)

testOutliers(simulationOutput_m2_total)

testZeroInflation(simulationOutput_m2_total)

testUniformity(simulationOutput_m2_total)

plot(simulationOutput_m2_total)

################################################################################
################################################################################

#MODELS ANNUAL EFFORT

#Most complex model
m2_year <- glmer(Neutered ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#check VIF
vif(m2_year) #all good

#check drop 1
drop1(m2_year, test = "Chisq") #drop 4y ago

#updated model
m2.1_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                     (1 | polygon),
                   family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))
#check drop 1 
drop1(m2.1_year, test = "Chisq")#drop owned 

#updated model
m2.2_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict + sex +
                     (1 | polygon),
                   family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))

#check drop 1
drop1(m2.2_year, test = "Chisq") #drop subdistrict

#updated model
m2.3_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + sex +
                     (1 | polygon),
                   family = binomial, data = sightings_ster, control = glmerControl(optimizer = "bobyqa"))

#check drop 1
drop1(m2.3_year, test = "Chisq") #all significant

################################################################################

#Check fit

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.3_year)

#Check with DHARMa
simulationOutput_m2_year <- simulateResiduals(fittedModel = m2.3_year) #create simulated data

testDispersion(simulationOutput_m2_year)

testOutliers(simulationOutput_m2_year)

testZeroInflation(simulationOutput_m2_year)

testUniformity(simulationOutput_m2_year)

plot(simulationOutput_m2_year)









################################################################################
################################################################################

##Presence of puppies model

#Most complex - since intervention
m3.2_since <- glm(Puppy ~ since_intervention + owned + subdistrict,
                  family = binomial, data = sightings_no_re)
vif(m3.2_since) #all fine
drop1(m3.2_since, test = "Chisq") #drop owned

#Updated model
m3.2.1_since <- glm(Puppy ~ since_intervention + subdistrict,
                  family = binomial, data = sightings_no_re)
drop1(m3.2.1_since, test = "Chisq") #both significant

summary(m3.2.1_since) #since intervention is not significant 

################################################################################

#most complex model - total effort
m3.2_effort <- glm(Puppy ~ effort_humanpop + owned + subdistrict,
                   family = binomial, data = sightings_no_re)
vif(m3.2_effort) #all fine
drop1(m3.2_effort, test = "Chisq") #drop owned

#updated model
m3.2.1_effort <- glm(Puppy ~ effort_humanpop + subdistrict,
                    family = binomial, data = sightings_no_re)
drop1(m3.2.1_effort, test = "Chisq") #drop effort

#updated model
m3.2.2_effort <- glm(Puppy ~ subdistrict,
                     family = binomial, data = sightings_no_re)
drop1(m3.2.2_effort, test = "Chisq") #no effort measures significant

################################################################################

#most complex model - annual effort
m3.2_year <- glm(Puppy ~ effort_4y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict,
                 family = binomial, data = sightings_no_re)

#check vif
vif(m3.2_year) #drop effort 1y
m3.2_year <- glm(Puppy ~ effort_4y_humanpop + effort_2y_humanpop + owned + subdistrict,
                 family = binomial, data = sightings_no_re)
#drop 1 test
drop1(m3.2_year, test = "Chisq")#4y

#updated model
m3.2.1_year <- glm(Puppy ~ effort_2y_humanpop + owned + subdistrict,
                 family = binomial, data = sightings_no_re)
#drop 1 test
drop1(m3.2.1_year, test = "Chisq")#drop owned

#updated model
m3.2.2_year <- glm(Puppy ~ effort_2y_humanpop + subdistrict,
                   family = binomial, data = sightings_no_re)

#drop 1 test
drop1(m3.2.2_year, test = "Chisq") #no effort measures significant

################################################################################

##COMPARE AIC##

#Dog Density
AIC(m1.1_since, m1.1_total, m1.3_year)#since intervention model has lowest AIC
AIC(m2.2_since, m2.2_total, m2.3_year)#annual effort model has lowest AIC
#no significant measures of effort variables in puppy model


#reflects similar results as to model with with both days of data (all same findings except puppy)

################################################################################
