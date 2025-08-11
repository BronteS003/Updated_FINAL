################################################################################
#             Model 4: Heath Status                                            #
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


##Model Selection - Health Status (sightings)##

#Time Since Intervention

#Most complex model using time since intervention
m4_since <- glmer(Healthy ~ since_intervention + owned + sex + age + Neutered + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_since, test = "Chisq")##None significantly improve the fit of the model 

#Total Sterilization Effort

#Most complex model using total sterilization effort
m4_total <- glmer(Healthy ~ effort_humanpop + owned + sex + age + Neutered + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_total, test = "Chisq") #None significantly improve the fit of the model

#Sterilization Effort by Year

#Most complex model using total sterilization effort
m4_year <- glmer(Healthy ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + owned + sex + age + Neutered + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_year, test = "Chisq") #None significantly improve the fit of the model

#Updated model dropping neutered
m4_1year <- glmer(Healthy ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + owned + sex + age + subdistrict +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_1year, test = "Chisq")

#Updated model dropping owned
m4_2year <- glmer(Healthy ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + sex + age + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m4_2year, test = "Chisq")

#Final model for health status
m4_final <- glmer(Healthy ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + sex + age + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))



##Check for overdispersion##

#Check residual deviance relative to degrees of freedom for model4.2
overdisp.glmer(m4_final)

#Visually check overdispersion using DHARMa plot
simulationOutput_model4 <- simulateResiduals(fittedModel = m4_final) #create simulated data
testDispersion(simulationOutput_model4)

#Test for outliers
testOutliers(simulationOutput_model4)

#Check for zero inflation
testZeroInflation(simulationOutput_model4)


##Graph model

# Get predicted values over 'sex' and 'subdistrict'
preds4 <- ggpredict(m4_final, terms = c("sex", "subdistrict")) #create predicted values over variables "since_intervention" and "subdistrict"

# Plot predicted data
ggplot(preds4, aes(x = x, y = predicted, color = group)) + #plot predicted data as the response variable, with group (meaning subdistrict) identified by colour
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) + 
  labs(
    title = "Predicted Probability of Being Healthy by Time Since Intervention and Subdistrict",
    x = "Days Since Intervention",
    y = "Probability of Being Healthy",
    color = "Subdistrict",
    fill = "Subdistrict"
  ) +
  theme_minimal()
