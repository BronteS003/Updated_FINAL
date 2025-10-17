################################################################################
#             FULL Model 4: Heath Status                                       #
################################################################################
# FULL data from sight and resight surveys as of Oct. 2025                     #
################################################################################
# Created Oct. 16, 2025 by Bronte Slote, last modified Oct. 16, 2025           #
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

summary_data <- readRDS("FULL_dog_density.rds", refhook = NULL)

##Organize variables##

#Create owned as proportion
summary_data$prop_owned <- summary_data$Owned / 
  (summary_data$Owned + summary_data$Free.roaming.NO.collar)

#Create Female column
summary_data <- summary_data %>%
  mutate(Female = rowSums(across(c(Adult.NON.lactating.female, Adult.Lactating.female)), na.rm = TRUE))

#Create sex as a proportion
summary_data$prop_female <- summary_data$Female / 
  (summary_data$Female + summary_data$Adult.male)

#Create column "Adult"
summary_data <- summary_data %>%
  mutate(Adult = rowSums(across(c(Adult.male, Adult.NON.lactating.female, Adult.Lactating.female)), na.rm = TRUE))

#Create age as a proportion
summary_data$prop_adult <- summary_data$Adult / 
  (summary_data$Adult + summary_data$Puppy)

#Create neutered as a proportion
summary_data$prop_neutered <- summary_data$Neutered / 
  (summary_data$Neutered + summary_data$Entire)

##Model Selection - Health Status (SUMMARY)##

#Time Since Intervention

#Most complex model using time since intervention
m4_since <- glmer(cbind(Healthy,Sick.or.injured) ~ since_intervention + prop_owned + prop_female + prop_adult + subdistrict +
                    (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m4_since)#all good
drop1(m4_since, test = "Chisq") 

#Create updated model dropping since
m4_1since<- glmer(cbind(Healthy,Sick.or.injured) ~ prop_owned + prop_female + prop_adult + subdistrict +
                    (1 | polygon/survey),
                  family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_1since, test = "Chisq")

#Create updated model dropping subdistrict
m4_2since <- glmer(cbind(Healthy,Sick.or.injured) ~ prop_owned + prop_female + prop_adult +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_2since, test = "Chisq")

#Create updated model dropping ownership status
m4_3since <- glmer(cbind(Healthy,Sick.or.injured) ~ prop_female + prop_adult +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_3since, test = "Chisq")

#Create updated model dropping sex
m4_4since <- glmer(cbind(Healthy,Sick.or.injured) ~ prop_adult +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_4since, test = "Chisq")



#Total Sterilization Effort

#Most complex model using total sterilization effort
m4_total <- glmer(cbind(Healthy,Sick.or.injured) ~ effort_humanpop + prop_owned + prop_female + prop_adult + subdistrict +
                    (1 | polygon/survey),
                  family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m4_total)#all good
drop1(m4_total, test = "Chisq") 

#Drop effort
m4_1total <- glmer(cbind(Healthy,Sick.or.injured) ~ prop_owned + prop_female + prop_adult + subdistrict +
                    (1 | polygon/survey),
                  family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_1total, test = "Chisq")

#Drop subdistrict
m4_2total <- glmer(cbind(Healthy,Sick.or.injured) ~ prop_owned + prop_female + prop_adult +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_2total, test = "Chisq")

#Drop owned
m4_3total <- glmer(cbind(Healthy,Sick.or.injured) ~ prop_female + prop_adult +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_3total, test = "Chisq")

#Drop female
m4_4total <- glmer(cbind(Healthy,Sick.or.injured) ~ prop_adult +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_4total, test = "Chisq") #Not significant


#Sterilization Effort by Year

#Most complex model using total sterilization effort
m4_year <- glmer(cbind(Healthy,Sick.or.injured) ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop +prop_owned + prop_female + prop_adult + subdistrict +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m4_year)#all fine
drop1(m4_year, test = "Chisq")

#Updated model dropping 1y
m4_1year<- glmer(cbind(Healthy,Sick.or.injured) ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + prop_owned + prop_female + prop_adult + subdistrict +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_1year, test = "Chisq")

#Updated model dropping 3y
m4_2year<- glmer(cbind(Healthy,Sick.or.injured) ~ effort_4y_humanpop + effort_2y_humanpop + prop_owned + prop_female + prop_adult + subdistrict +
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_2year, test = "Chisq")

#Updated model dropping subdistrict
m4_3year<- glmer(cbind(Healthy,Sick.or.injured) ~ effort_4y_humanpop + effort_2y_humanpop + prop_owned + prop_female + prop_adult + 
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_3year, test = "Chisq")

#Updated model dropping 4y
m4_4year<- glmer(cbind(Healthy,Sick.or.injured) ~ effort_2y_humanpop + prop_owned + prop_female + prop_adult + 
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_4year, test = "Chisq")

#Updated model dropping 2y
m4_5year<- glmer(cbind(Healthy,Sick.or.injured) ~ prop_owned + prop_female + prop_adult + 
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_5year, test = "Chisq")

#Updated model dropping owned
m4_6year<- glmer(cbind(Healthy,Sick.or.injured) ~ prop_female + prop_adult + 
                   (1 | polygon/survey),
                 family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_6year, test = "Chisq")#None significant



##Model Selection - Health Status (INDIVIDUAL)##

#Import data
sightings <- readRDS("FULL_sightings.rds", refhook = NULL)

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

#Updated model, dropping neutered
m4_2since <- glmer(Healthy ~ since_intervention + sex + age + owned +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_2since, test = "Chisq")

#Updated model, dropping owned
m4_3since <- glmer(Healthy ~ since_intervention + sex + age + 
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_3since, test = "Chisq")

#Updated model, dropping since
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

#Updated model dropping subdistrict
m4_total <- glmer(Healthy ~ effort_humanpop + sex + age + Neutered + owned + 
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")

#Updated model dropping Neutered
m4_total <- glmer(Healthy ~ effort_humanpop + sex + age + owned + 
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")

#Updated model dropping owned
m4_total <- glmer(Healthy ~ effort_humanpop + sex + age +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")

#Updated model dropping effort
m4_total <- glmer(Healthy ~ sex + age +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")

#Updated model dropping age
m4_total <- glmer(Healthy ~ sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq")#Not significant


##Years

#Most complex model
m4_year <- glmer(Healthy ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + sex + age + Neutered + owned + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m4_year)#all good
drop1(m4_year, test = "Chisq")

#Create updated model dropping y3
m4_year <- glmer(Healthy ~ effort_4y_humanpop + effort_2y_humanpop + effort_1y_humanpop + sex + age + Neutered + owned + subdistrict +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping 2y
m4_year <- glmer(Healthy ~ effort_4y_humanpop + effort_1y_humanpop + sex + age + Neutered + owned + subdistrict +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping subdistrict
m4_year <- glmer(Healthy ~ effort_4y_humanpop + effort_1y_humanpop + sex + age + Neutered + owned +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping 1y
m4_year <- glmer(Healthy ~ effort_4y_humanpop + sex + age + Neutered + owned +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping owned
m4_year <- glmer(Healthy ~ effort_4y_humanpop + sex + age + Neutered +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping Neutered
m4_year <- glmer(Healthy ~ effort_4y_humanpop + sex + age + 
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

#Create updated model dropping effort
m4_year <- glmer(Healthy ~ sex + age + 
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq")

