################################################################################
#             Model 4: Heath Status                                            #
################################################################################
# Data from sight and resight surveys as recorded in WVS                       #
################################################################################
# Created June 14, 2025 by Bronte Slote, last modified Aug. 21, 2025           #
################################################################################

##Load Libraries
library(readr) #reading csv files
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

##IMPORT DATA##

#Read rds for sightings file

summary_data <- readRDS("dog_density.rds", refhook = NULL)



##Model Selection - Health Status (SUMMARY)##

#Time Since Intervention

#Most complex model using time since intervention
m4_since <- glmer(cbind(Healthy,Sick.or.injured) ~ since_intervention + Owned + Adult.male + Puppy + Neutered + subdistrict +
                    (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m4_since)#drop neutered
m4_since <- glmer(cbind(Healthy,Sick.or.injured) ~ since_intervention + Owned + Adult.male + Puppy + subdistrict +
                    (1 | polygon/survey),
                  family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_since, test = "Chisq") 

#Create updated model dropping since
m4_1since<- glmer(cbind(Healthy,Sick.or.injured) ~ Owned + Adult.male + Puppy + subdistrict +
                    (1 | polygon/survey),
                  family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_1since, test = "Chisq")

#Create updated model dropping sex
m4_2since <- glmer(cbind(Healthy,Sick.or.injured) ~ Owned + Puppy + subdistrict +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_2since, test = "Chisq")

#Create updated model dropping ownership status
m4_3since <- glmer(cbind(Healthy,Sick.or.injured) ~ Puppy + subdistrict +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_3since, test = "Chisq")

#Create updated model dropping puppy
m4_4since <- glmer(cbind(Healthy,Sick.or.injured) ~ subdistrict +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_4since, test = "Chisq")



#Total Sterilization Effort

#Most complex model using total sterilization effort
m4_total <- glmer(cbind(Healthy,Sick.or.injured) ~ effort_humanpop + Owned + Adult.male + Puppy + Neutered + subdistrict +
                    (1 | polygon/survey),
                  family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m4_total)#drop neutered
m4_total <- glmer(cbind(Healthy,Sick.or.injured) ~ effort_humanpop + Owned + Adult.male + Puppy + subdistrict +
                    (1 | polygon/survey),
                  family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq") #None significant



#Sterilization Effort by Year

#Most complex model using total sterilization effort
m4_year <- glmer(cbind(Healthy,Sick.or.injured) ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + Owned + Adult.male + Puppy + Neutered + subdistrict +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m4_year)#all fine
drop1(m4_year, test = "Chisq")

#Updated model dropping sex
m4_1year<- glmer(cbind(Healthy,Sick.or.injured) ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + Owned + Puppy + Neutered + subdistrict +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_1year, test = "Chisq")

#Updated model dropping puppy
m4_2year<- glmer(cbind(Healthy,Sick.or.injured) ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + Owned + Neutered + subdistrict +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_2year, test = "Chisq")

#Updated model dropping subdistrict
m4_3year<- glmer(cbind(Healthy,Sick.or.injured) ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + Owned + Neutered +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_3year, test = "Chisq")

#Updated model dropping 1y
m4_4year<- glmer(cbind(Healthy,Sick.or.injured) ~ effort_3y_humanpop + effort_2y_humanpop + Owned + Neutered +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_4year, test = "Chisq")

#Updated model dropping 3y
m4_4year<- glmer(cbind(Healthy,Sick.or.injured) ~ effort_2y_humanpop + Owned + Neutered +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_4year, test = "Chisq")

#Updated model dropping 2y
m4_4year<- glmer(cbind(Healthy,Sick.or.injured) ~ Owned + Neutered +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_4year, test = "Chisq")#None significant



##Model Selection - Health Status (INDIVIDUAL)##

#Import data
sightings <- readRDS("sightings.rds", refhook = NULL)

##Since Intervention

#Most complex model
m4_since <- glmer(Healthy ~ since_intervention + sex + age + Neutered + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m4_since)#all good
drop1(m4_since, test = "Chisq")

#Updated model, dropping subdistrict
m4_1since <- glmer(Healthy ~ since_intervention + sex + age + Neutered + owned +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_1since, test = "Chisq")

#Updated model, dropping since
m4_2since <- glmer(Healthy ~ sex + age + Neutered + owned +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_2since, test = "Chisq")

#Updated model, dropping neutered
m4_3since <- glmer(Healthy ~ sex + age + owned +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_3since, test = "Chisq")

#Updated model, dropping owned
m4_3since <- glmer(Healthy ~ sex + age +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_3since, test = "Chisq")

#Updated model, dropping age
m4_4since <- glmer(Healthy ~ sex +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_4since, test = "Chisq")## Not significant



##Total Effort

#Most complex model
m4_total <- glmer(Healthy ~ effort_humanpop + sex + age + Neutered + owned + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m4_total)#all good
drop1(m4_total, test = "Chisq")

#Updated model dropping effort
m4_total <- glmer(Healthy ~ sex + age + Neutered + owned + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")

#Updated model dropping subdistrict
m4_total <- glmer(Healthy ~ sex + age + Neutered + owned +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")

#Updated model dropping neutered
m4_total <- glmer(Healthy ~ sex + age + owned +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")

#Updated model dropping owned
m4_total <- glmer(Healthy ~ sex + age +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")

#Updated model dropping owned
m4_total <- glmer(Healthy ~ sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")#Not significant


##Years

#Most complex model
m4_year <- glmer(Healthy ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + sex + age + Neutered + owned + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m4_year)#drop 2y
m4_year <- glmer(Healthy ~ effort_3y_humanpop + effort_1y_humanpop + sex + age + Neutered + owned + subdistrict +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping y1
m4_year <- glmer(Healthy ~ effort_3y_humanpop + sex + age + Neutered + owned + subdistrict +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping subdistrict
m4_year <- glmer(Healthy ~ effort_3y_humanpop + sex + age + Neutered + owned +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping 3y
m4_year <- glmer(Healthy ~ sex + age + Neutered + owned +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping neutered
m4_year <- glmer(Healthy ~ sex + age + owned +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping owned
m4_year <- glmer(Healthy ~ sex + age +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping owned
m4_year <- glmer(Healthy ~ sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")##None significant

