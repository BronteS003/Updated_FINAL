################################################################################
#             Model 2: Change in Sterilization Status Overtime                 #
################################################################################
# Data from sight and resight surveys as recorded in WVS                       #
################################################################################
# Created June 7, 2025 by Bronte Slote, last modified July 17, 2025             #
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
library(car) #check for multicollinearity
library(performance)


##IMPORT DATA##

#Read rds for sightings file

sightings <- readRDS("sightings.rds", refhook = NULL)

##MODEL SELECTION##

#Time Since Intervention

#Create first model using time since intervention
m2_since <- glmer(Neutered ~ since_intervention + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_since, test = "Chisq")

#Final model for time since intervention
m2_since_final<- glmer(Neutered ~ since_intervention + owned + subdistrict + sex +
                         (1 | polygon),
                       family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#Total Effort

#Create first model using total effort
m2_total <- glmer(Neutered ~ effort_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_total, test = "Chisq")

#Final model for total effort
m2_total_final <- glmer(Neutered ~ effort_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))


#Effort by year

#Create first model using effort by year
m2_year <- glmer(Neutered ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_year, test = "Chisq")

#Final model for effort by year
m2_year_final <- glmer(Neutered ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))


#Compare three sterilization effort indicators
AIC(m2_since_final, m2_total_final, m2_year_final) #year model has the lowest AIC


##CHECK FOR OVERDISPERSION##

#Visually check overdispersion using DHARMa plot
simulationOutput_model2 <- simulateResiduals(fittedModel = m2_year_final) #create simulated data
testDispersion(simulationOutput_model2)

#Test for outliers
testOutliers(simulationOutput_model2)

#Check for zero inflation
testZeroInflation(simulationOutput_model2)



##PLOT MODELS##

#Trying to figure out how to plot this in a way that makes sense

# Get predicted values over 'last3y_humanpop'and 'subdistrict'
preds_3y <- ggpredict(m2_year_final, terms = c("last3y_humanpop", "subdistrict"))

# Plot Probability of Being Neutered by Sterilizations 3 years ago and subdistrict
ggplot(preds_3y, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Probability of Being Neutered by Sterilization Effort (3 years ago) and Subdistrict",
    x = "Sterilization Effort",
    y = "Probability of Being Neutered",
    color = "Subdistrict",
    fill = "Subdistrict"
  ) +
  theme_minimal()


# Get predicted values over "owned" and "subdistrict"
preds_owned <- ggpredict(m2_year_final, terms = c("owned", "subdistrict"))

# Plot Probability of Being Neutered by Sterilizations ownership status and subdistrict
ggplot(preds_owned, aes(x = x, y = predicted, fill = group)) +
  geom_boxplot(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(0.9), width = 0.2) +
  labs(
    title = "Predicted Probability of Being Neutered by Ownership Status and Subdistrict",
    x = "Owned",
    y = "Probability of Being Neutered",
    fill = "Subdistrict"
  ) +
  theme_minimal()


# Get predicted values over "sex" and "subdistrict"
preds_sex <- ggpredict(m2_year_final, terms = c("sex", "subdistrict"))

# Plot Probability of Being Neutered by Sterilizations ownership status and subdistrict
ggplot(preds_sex, aes(x = x, y = predicted, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(0.9), width = 0.2) +
  labs(
    title = "Predicted Probability of Being Neutered by Ownership Status and Subdistrict",
    x = "Sex",
    y = "Probability of Being Neutered",
    fill = "Subdistrict"
  ) +
  theme_minimal()
