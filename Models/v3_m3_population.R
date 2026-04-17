################################################################################
##        MODEL 3.1 AND 3.2 - LACTATING FEMALES AND PUPPIES                   ##
################################################################################
# Predicting the probability that a female dog is lactating (m3.1) and that a  #
# dog is a puppy.                                                                #
################################################################################
# Created April 16, 2026 by Bronte Slote last edited April 16, 2026            #
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
library(car) #checking vif
library(scales) #axis and legend label appearances

################################################################################

##LOAD RDS ##

#load RDS file
sightings <- readRDS("Data/sightings_v3.rds")

################################################################################

##CLEAN DATA - Lactating Females##

#Remove puppies as they're not relevant to analysis
pop_sightings <- sightings %>% 
  filter(Puppy != 1)

#Remove males
pop_sightings <- pop_sightings %>% 
  filter(Adult.male != 1)

################################################################################

##MODEL SELECTION LACTATING FEMALES - SINCE INTERVENTION##

#Most complex model, lactating females by time since intervention
m3.1_since <- glmer(Adult.Lactating.female ~ since_intervention + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_since)#all fine
drop1(m3.1_since, test = "Chisq") 

#Create updated model dropping owned
m3.1_1since <- glmer(Adult.Lactating.female ~ since_intervention + subdistrict +
                       (1 | polygon),
                     family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1since, test = "Chisq") 

#Create updated model dropping subdistrict
m3.1_2since <- glmer(Adult.Lactating.female ~ since_intervention +
                       (1 | polygon),
                     family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2since, test = "Chisq") #None of the variables are significant

################################################################################

##MODEL SELECTION LACTATING FEMALES - TOTAL EFFORT##

#Most complex model
m3.1_effort <- glmer(Adult.Lactating.female ~ sc_total + owned + subdistrict +
                       (1 | polygon),
                     family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_effort) #all fine
drop1(m3.1_effort, test = "Chisq")#drop owned

#Updated model dropping owned
m3.1.1_effort <- glmer(Adult.Lactating.female ~ sc_total + subdistrict +
                       (1 | polygon),
                     family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1.1_effort) #all fine
drop1(m3.1.1_effort, test = "Chisq")#drop owned

#Updated model dropping total
m3.1.2_effort <- glmer(Adult.Lactating.female ~ subdistrict +
                         (1 | polygon),
                       family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1.2_effort, test = "Chisq")#None of the variables are significant

################################################################################

##MODEL SELECTION LACTATING FEMALES - YEARLY EFFORT##

#most complex model
m3.1_year <- glmer(Adult.Lactating.female ~ sc_4y + sc_3y + sc_2y + sc_1y + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m3.1_year)
vif(m3.1_year) #bad multicollinearity between yearly effort variables, drop 2y
m3.1_year <- glmer(Adult.Lactating.female ~ sc_4y + sc_3y + sc_1y + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_year) #all good now
drop1(m3.1_year, test = "Chisq") #drop 4y

#updated model dropping 4y
m3.1.2_year <- glmer(Adult.Lactating.female ~ sc_3y + sc_2y + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1.2_year, test = "Chisq") #drop owned

#updated model dropping owned
m3.1.3_year <- glmer(Adult.Lactating.female ~ sc_3y + sc_2y + subdistrict +
                       (1 | polygon),
                     family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1.3_year, test = "Chisq") #drop 2y ago

#updated model dropping 2y ago
m3.1.4_year <- glmer(Adult.Lactating.female ~ sc_3y + subdistrict +
                       (1 | polygon),
                     family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1.4_year, test = "Chisq") #Drop subdistrict

#updated model dropping subdistrict
m3.1.5_year <- glmer(Adult.Lactating.female ~ sc_3y +
                       (1 | polygon),
                     family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1.5_year, test = "Chisq") #None significant

################################################################################

##MODEL SELECTION PUPPIES - SINCE INTERVENTION##

#Most complex models
m3.2_since <- glm(Puppy ~ since_intervention + owned + subdistrict,
                    family = binomial, data = sightings)
vif(m3.2_since) #all fine
drop1(m3.2_since, test = "Chisq") #drop owned

#Updated model dropping owned
m3.2.1_since <- glm(Puppy ~ since_intervention + subdistrict,
                    family = binomial, data = sightings)
drop1(m3.2.1_since, test = "Chisq") #both significant

################################################################################

##CHECK OVERDISPERSION##

#Check with DHARMa
simulationOutput_m3_since <- simulateResiduals(fittedModel = m3.2.1_since) #create simulated data

testDispersion(simulationOutput_m3_since)

testOutliers(simulationOutput_m3_since)

testZeroInflation(simulationOutput_m3_since)

testUniformity(simulationOutput_m3_since)

plot(simulationOutput_m3_since)

################################################################################

##MODEL SELECTION PUPPIES - TOTAL EFFORT##

#Most complex model
m3.2_effort <- glm(Puppy ~ effort_all_time + owned + subdistrict,
                     family = binomial, data = sightings)
vif(m3.2_effort) #all fine
drop1(m3.2_effort, test = "Chisq") #all significant

################################################################################

##CHECK OVERDISPERSION##

#Check with DHARMa
simulationOutput_m3_effort <- simulateResiduals(fittedModel = m3.2_effort) #create simulated data

testDispersion(simulationOutput_m3_effort)

testOutliers(simulationOutput_m3_effort)

testZeroInflation(simulationOutput_m3_effort)

testUniformity(simulationOutput_m3_effort)

plot(simulationOutput_m3_effort)

################################################################################

##MODEL SELECTION PUPPIES - YEARLY EFFORT##

#Most complex model
m3.2_year <- glm(Puppy ~ effort_4y_ago + effort_2y_ago + effort_1y_ago + owned + subdistrict,
                   family = binomial, data = sightings)
#effort 3y ago is a problem - fitted probabilities numerically 0 or 1 occurred, so removed
vif(m3.2_year) #bad multicollinearity between yearly effort variables, drop 1y
m3.2_year <- glm(Puppy ~ effort_4y_ago + effort_2y_ago + owned + subdistrict,
                 family = binomial, data = sightings)
vif(m3.2_year) #all good now
drop1(m3.2_year, test = "Chisq") #drop 4y ago

#Updated model dropping 4y ago
m3.2.1_year <- glm(Puppy ~ effort_2y_ago + owned + subdistrict,
                   family = binomial, data = sightings)
drop1(m3.2.1_year, test = "Chisq") #drop 3y ago

#Updated model dropping owned
m3.2.2_year <- glm(Puppy ~ effort_2y_ago + subdistrict,
                   family = binomial, data = sightings)
drop1(m3.2.2_year, test = "Chisq") #all significant

################################################################################

##CHECK OVERDISPERSION##

#Check with DHARMa
simulationOutput_m3_year <- simulateResiduals(fittedModel = m3.2.2_year) #create simulated data

testDispersion(simulationOutput_m3_year)

testOutliers(simulationOutput_m3_year)

testZeroInflation(simulationOutput_m3_year)

testUniformity(simulationOutput_m3_year)

plot(simulationOutput_m3_year)

################################################################################

##COMPARE MODELS##

#Compare AIC values
AIC(m3.2.1_since, m3.2_effort, m3.2.2_year) #all very similar

################################################################################

##PLOT MODELS##

#Plot since intervention model
preds_since <- ggpredict(m3.2.1_since, terms = c("since_intervention", "subdistrict"))

plot(preds_since) +
  labs(x = "Time since intervention (years)", y = "Predicted probability of being a puppy", color = "Subdistrict", title ="") +
  theme_minimal() +
  theme(legend.position = "bottom",,
        legend.text = element_text(size = 9))

#Plot total effort model
preds_effort <- ggpredict(m3.2_effort, terms = c("effort_all_time", "subdistrict"))
preds_owned <- ggpredict(m3.2_effort, terms = c("owned"))

plot(preds_effort) +
  labs(x = "Total Sterilizations", y = "Predicted probability of being a puppy", color = "Subdistrict", title ="") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9))

plot(preds_owned) +
  labs(x = "Owned", y = "Predicted probability of being a puppy", title ="") +
  theme_minimal()

#Plot yearly effort model
preds_year <- ggpredict(m3.2.2_year, terms = c("effort_2y_ago", "subdistrict"))

plot(preds_year) +
  labs(x = "Sterilizations 2 Years Ago", y = "Predicted probability of being a puppy", color = "Subdistrict", title ="") +
  theme_minimal() +
  theme(legend.position = "bottom",,
        legend.text = element_text(size = 9))
