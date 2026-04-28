################################################################################
#             FULL Model 4: Heath Status                                       #
################################################################################
# FULL data from sight and resight surveys as of Oct. 2025                     #
################################################################################
# Created Oct. 16, 2025 by Bronte Slote, last modified Apr. 27, 2026           #
################################################################################

##LOAD LIBRARIES##
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(lme4) #creating mixed models with random effect
library(stringr) #manipulating text
library(RVAideMemoire) #checking for overdispersion
library(DHARMa) #checking overdispersion visually
library(ggeffects) #creating predicted values and visualizing them
library(lmtest) #conducting likelihood ratio tests
library(tidyr) #cleaning data
library(patchwork) #combining plots into one panel
library(emmeans) #model comparisons
library(car) #checking vif

################################################################################

##LOAD DATA##
sightings <- readRDS("Data/sightings_v2.rds")

################################################################################

##MODEL SELECTION SINCE INTERVENTION##

#Most complex model 
m4_since <- glmer(Healthy ~ since_intervention + sex + age + Neutered + owned + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m4_since)

#check VIF
vif(m4_since) #all good

#drop 1 test
drop1(m4_since, test = "Chisq") #drop subdistrict

#updated model dropping subdistrict
m4.1_since <- glmer(Healthy ~ since_intervention + sex + age + Neutered + owned + 
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m4.1_since)

#drop 1 test
drop1(m4.1_since, test = "Chisq") #drop neutered

#updated model dropping neutered
m4.2_since <- glmer(Healthy ~ since_intervention + sex + age + owned + 
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#drop 1 test
drop1(m4.2_since, test = "Chisq") #drop owned

#updated model dropping owned
m4.3_since <- glmer(Healthy ~ since_intervention + sex + age + 
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#drop 1 test
drop1(m4.3_since, test = "Chisq") #drop since intervention

#updated model dropping since intervention
m4.4_since <- glmer(Healthy ~ sex + age + 
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
#drop 1 test
drop1(m4.4_since, test = "Chisq") #drop age

#updated model dropping age
m4.5_since <- glmer(Healthy ~ sex + 
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m4.5_since) #not significant

################################################################################

##MODEL SELECTION - TOTAL EFFORT##

#Most complex model
m4_total <- glmer(Healthy ~ effort_humanpop +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m4_total)#not significant

################################################################################

##MODEL SELECTION -YEARLY EFFORT##

#Most complex model
m4_year <- glmer(Healthy ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#Check VIF
vif(m4_year) #all good

#Drop 1 test
drop1(m4_year, test = "Chisq") #drop 2 years ago

#Updated model dropping 2 years ago
m4.1_year <- glmer(Healthy ~ effort_4y_humanpop + effort_3y_humanpop + effort_1y_humanpop +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
#Drop 1 test
drop1(m4.1_year, test = "Chisq") #drop 3 years

#Updated model dropping 3 years ago
m4.2_year <- glmer(Healthy ~ effort_4y_humanpop + effort_1y_humanpop +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
#Drop 1 test
drop1(m4.2_year, test = "Chisq") #drop 1

#Updated model dropping 1 year ago
m4.3_year <- glmer(Healthy ~ effort_4y_humanpop +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

summary(m4.3_year) #not significant
