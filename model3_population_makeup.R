################################################################################
#             Model 3: Presence of Lactating Females and Puppies               #
################################################################################
# Data from sight and resight surveys as recorded in WVS                       #
################################################################################
# Created June 14, 2025 by Bronte Slote, last modified July 15, 2025           #
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

##IMPORT DATA##

#Read rds for sightings file

sightings <- readRDS("sightings.rds", refhook = NULL)

##MODEL SELECTION (Sightings) - Lactating Females##

#Time since intervention

#Most complex model, lactating females by time since intervention
m3.1_since <- glmer(Adult.Lactating.female ~ since_intervention + owned + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_since, test = "Chisq") ##None significantly improve fit of the model

#Total Effort

#Most complex model, lactating females by total sterilization effort
m3.1_effort <- glmer(Adult.Lactating.female ~ effort_humanpop + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_effort, test = "Chisq") ##None significantly improve fit of the model

#Effort by year

#Most complex model, lactating females by effort annually
m3.1_year <- glmer(Adult.Lactating.female ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + owned + subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_year, test = "Chisq") ##None are significantly improve fit of the model




##MODEL SELECTION (Dog Density) - Lactating Females##
dog_density <- readRDS("dog_density.rds", refhook = NULL)

#Time since intervention

#Most complex model, lactating females by time since intervention
m3.1_since_dd <- glmer(Adult.Lactating.female ~ since_intervention + Owned + Free.roaming.NO.collar + subdistrict +
                      (1 | polygon/survey),
                    family = poisson, data = dog_density, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_since_dd, test = "Chisq")

#Updated model droppping free roaming no collar
m3.1_1since_dd <- glmer(Adult.Lactating.female ~ since_intervention + Owned + subdistrict +
                         (1 | polygon/survey),
                       family = poisson, data = dog_density, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1since_dd, test = "Chisq") ##None significantly improve fit of the model


#Total Effort

#Most complex model, lactating females by total sterilization effort
m3.1_effort_dd <- glmer(Adult.Lactating.female ~ effort_humanpop + Owned + Free.roaming.NO.collar + subdistrict +
                         (1 | polygon/survey),
                       family = poisson, data = dog_density, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_effort_dd, test = "Chisq") ##None significantly improve fit of the model


#Effort by year

#Most complex model, lactating females by effort annually
m3.1_year_dd <- glmer(Adult.Lactating.female ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + Owned + Free.roaming.NO.collar + subdistrict +
                          (1 | polygon/survey),
                        family = poisson, data = dog_density, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_year_dd, test = "Chisq") ##None are significantly improve fit of the model


##MODEL SELECTION (sightings) - Lactating Females##

#Time since intervention

#Most complex model, puppies by time since intervention
m3.2_since <- glmer(Puppy ~ since_intervention + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_since, test = "Chisq")

#Final model for time since intervention 
m3.2_since_final <- glmer(Puppy ~ since_intervention + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))


#Total Effort

#Most complex model, puppies by total sterilization effort
m3.2_effort <- glmer(Puppy ~ effort_humanpop + owned + subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_effort, test = "Chisq") 

#Final model, puppies by total sterilization effort
m3.2_effort_final <- glmer(Puppy ~ effort_humanpop + owned + subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#Effort by year

#Most complex model, puppy by effort annually
m3.2_year <- glmer(Puppy ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_year, test = "Chisq") 

#Updated model dropping last 1 year
m3.2_1year <- glmer(Puppy ~ last3y_humanpop + last2y_humanpop + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_1year, test = "Chisq") 

#Updated model dropping last 2 year
m3.2_2year <- glmer(Puppy ~ last3y_humanpop + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_2year, test = "Chisq")

#Final model for year
m3.2_year_final <- glmer(Puppy ~ last3y_humanpop + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#Compare all final models
AIC(m3.2_since_final, m3.2_effort_final, m3.2_year_final) #nearly identical AICs, time since intervention marginally better



##Check for overdispersion
#--------------------

#Check residual deviance relative to degrees of freedom for model3.2
overdisp.glmer(m3.2_since_final)

#Visually check overdispersion using DHARMa plot
simulationOutput_model3.2 <- simulateResiduals(fittedModel = m3.2_since_final) #create simulated data
testDispersion(simulationOutput_model3.2)

#Test for outliers
testOutliers(simulationOutput_model3.2)

#Check for zero inflation
testZeroInflation(simulationOutput_model3.2)


##Graph model 3.2

# Get predicted values over 'since_intervention'and 'subdistrict'
preds3.2 <- ggpredict(m3.2_since_final, terms = c("since_intervention", "subdistrict"))

# Plot
ggplot(preds3.2, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Probability of Being a Puppy by Time Since Intervention and Subdistrict",
    x = "Years Since Intervention",
    y = "Probability of Being a Puppy",
    color = "Subdistrict",
    fill = "Subdistrict"
  ) +
  theme_minimal()
