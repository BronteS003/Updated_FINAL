################################################################################
##   Testing Effort Variable as an Interaction Term in the Model              ##
################################################################################
# Rerunning the model with effort as an interaction with subdistrict instead   #
# of dividing by subdistrict population.                                       #
################################################################################
# Created Apr. 27, 2026 by Bronte Slote, last edited Apr. 27, 2026             #
################################################################################

##LOAD LIBRARIES##
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(lme4) #creating mixed models with random effect
library(stringr) #manipulating text

################################################################################

##IMPORT DATA##

#Dog density
dog_density <- readRDS("Data/density_v2.rds")

#Sightings
sightings <- readRDS("Data/sightings_v2.rds")

################################################################################

## REFIT MODEL 1 ##

#m1 total effort
m1_effort <- glmer(Sighting.Count ~ effort_all_time*subdistrict + day + Mode.Transport +
                              (1 | polygon) +
                              offset(log(Track.Length)), 
                            family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

m1_year <- glmer(Sighting.Count ~ effort_4y_ago*subdistrict + effort_3y_ago*subdistrict + effort_2y_ago*subdistrict + effort_1y_ago*subdistrict + day + Mode.Transport +
                   (1 | polygon) +
                   offset(log(Track.Length)), 
                 family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

################################################################################

## REFIT MODEL 2 ##

#M2 total effort
m2_total <- glmer(Neutered ~ effort_all_time*subdistrict + owned + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#M2 year effort
m2_year <- glmer(Neutered ~ effort_4y_ago*subdistrict + effort_3y_ago*subdistrict + effort_2y_ago*subdistrict + effort_1y_ago*subdistrict + owned + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
