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
library(tidyr)

##IMPORT DATA##

#Read rds for sightings file

sightings <- readRDS("sightings.rds", refhook = NULL)

##CLEAN DATA##
#Remove puppies as they're not relevant to analysis
sightings <- sightings %>% 
  filter(Puppy != 1)

#Remove males
sightings <- sightings %>% 
  filter(Adult.male != 1)

##MODEL SELECTION (Sightings) - Lactating Females##

#Time since intervention

#Most complex model, lactating females by time since intervention
m3.1_since <- glmer(Adult.Lactating.female ~ since_intervention + owned + subdistrict +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_since)#all fine
drop1(m3.1_since, test = "Chisq") 

#Create updated model dropping owned
m3.1_1since <- glmer(Adult.Lactating.female ~ since_intervention + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1since, test = "Chisq") 

#Create updated model dropping since_intervention
m3.1_2since <- glmer(Adult.Lactating.female ~ subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2since, test = "Chisq") #None of the variables are significant



#Total Effort

#Most complex model, lactating females by total sterilization effort
m3.1_effort <- glmer(Adult.Lactating.female ~ effort_humanpop + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_effort)#all fine
drop1(m3.1_effort, test = "Chisq") 

#Create updated model dropping effort
m3.1_1effort <- glmer(Adult.Lactating.female ~ owned + subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1effort, test = "Chisq") 

#Create updated model dropping owned
m3.1_2effort <- glmer(Adult.Lactating.female ~ owned +
                        (1 | polygon),
                      family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2effort, test = "Chisq") #None of the variables are significant


#Effort by year

#Most complex model, lactating females by effort annually
m3.1_year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_year) #drop effort 2y
m3.1_year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop + effort_1y_humanpop + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_year) #all now fine

#Check which variable to drop
drop1(m3.1_year, test = "Chisq") 

#Create updated model dropping owned
m3.1_1year<- glmer(Adult.Lactating.female ~ effort_3y_humanpop + effort_1y_humanpop + subdistrict +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1year, test = "Chisq") 

#Create updated model dropping effort 1y
m3.1_2year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2year, test = "Chisq") 

#Create updated model dropping subdistrict
m3.1_3year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_3year, test = "Chisq") 


#Final model - lactating females
m3.1_final <- glmer(Adult.Lactating.female ~ effort_3y_humanpop +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))









##MODEL SELECTION (SUMMARY) - LACTATING FEMALES##
#Read rds for sightings file

summary_data <- readRDS("dog_density.rds", refhook = NULL)

##CLEAN DATA##
#Remove puppies as they're not relevant to analysis
summary_data$Puppy <- NULL

#Remove males
summary_data$Adult.male <- NULL

##MODEL SELECTION (Sightings) - Lactating Females##

#Time since intervention

#Most complex model, lactating females by time since intervention
m3.1_since <- glmer(Adult.Lactating.female ~ since_intervention + Owned + subdistrict +
                      (1 | polygon/survey),
                    family = poisson, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_since)#all fine
drop1(m3.1_since, test = "Chisq") 


#Total Effort

#Most complex model, lactating females by total sterilization effort
m3.1_effort <- glmer(Adult.Lactating.female ~ effort_humanpop+ Owned + subdistrict +
                       (1 | polygon/survey),
                     family = poisson, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_effort)#all fine
drop1(m3.1_effort, test = "Chisq") 

#Create updated model dropping subdistrict
m3.1_1effort <- glmer(Adult.Lactating.female ~ effort_humanpop + Owned +
                        (1 | polygon/survey),
                      family = poisson, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1effort, test = "Chisq") 


#Effort by year

#Most complex model, lactating females by effort annually
m3.1_year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + Owned + subdistrict +
                     (1 | polygon/survey),
                   family = poisson, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_year) #drop effort 1y
m3.1_year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop + effort_2y_humanpop + Owned + subdistrict +
                     (1 | polygon/survey),
                   family = poisson, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_year) #all now fine

#Check which variable to drop
drop1(m3.1_year, test = "Chisq") 

#Create updated model dropping subdistrict
m3.1_1year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop + effort_2y_humanpop + Owned +
                     (1 | polygon/survey),
                   family = poisson, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1year, test = "Chisq") 

#Create updated model dropping effort 2y
m3.1_2year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop + Owned +
                      (1 | polygon/survey),
                    family = poisson, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2year, test = "Chisq") 

#Create updated model dropping owned
m3.1_3year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop +
                      (1 | polygon/survey),
                    family = poisson, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_3year, test = "Chisq") 







##MODEL SELECTION (sightings) - Puppies##

#Reload fresh version of sightings file
sightings <- readRDS("sightings.rds", refhook = NULL)

#Time since intervention

#Most complex model, puppies by time since intervention
m3.2_since <- glmer(Puppy ~ since_intervention + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.2_since)
drop1(m3.2_since, test = "Chisq")

#Updated model dropping owned
m3.2_1since <- glmer(Puppy ~ since_intervention + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_1since, test = "Chisq")

#Updated model dropping subdistrict
m3.2_1since <- glmer(Puppy ~ since_intervention +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_1since, test = "Chisq")


#Total Effort

#Most complex model, puppies by total sterilization effort
m3.2_effort <- glmer(Puppy ~ effort_humanpop + owned + subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.2_effort)
drop1(m3.2_effort, test = "Chisq")

#Drop owned
m3.2_1effort <- glmer(Puppy ~ effort_humanpop + subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_1effort, test = "Chisq")

#Drop subdistrict
m3.2_2effort <- glmer(Puppy ~ effort_humanpop +
                        (1 | polygon),
                      family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_2effort, test = "Chisq")

#Final model, puppies by total sterilization effort
m3.2_effort_final <- glmer(Puppy ~ effort_humanpop +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#Effort by year

#Most complex model, puppy by effort annually
m3.2_year <- glmer(Puppy ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.2_year) #drop effort 2y
m3.2_year <- glmer(Puppy ~ effort_3y_humanpop + effort_1y_humanpop + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.2_year) #all good
drop1(m3.2_year, test = "Chisq") 

#Updated model dropping effort 1
m3.2_1year <- glmer(Puppy ~ effort_3y_humanpop + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_1year, test = "Chisq") 

#Updated model dropping subdistrict
m3.2_2year  <- glmer(Puppy ~ effort_3y_humanpop + owned +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_2year, test = "Chisq")

#Updated model dropping owned
m3.2_2year  <- glmer(Puppy ~ effort_3y_humanpop +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_2year, test = "Chisq")

#Final model for year
m3.2_year_final <- glmer(Puppy ~ effort_3y_humanpop +
                           (1 | polygon),
                         family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#Compare all final models
AIC(m3.2_effort_final, m3.2_year_final) #year fits the model better




##Check for overdispersion

#Check residual deviance relative to degrees of freedom for model3.2
overdisp.glmer(m3.1_final)

overdisp.glmer(m3.2_since_final)

#Visually check overdispersion using DHARMa plot
simulationOutput_model3.1 <- simulateResiduals(fittedModel = m3.1_final) #create simulated data
testDispersion(simulationOutput_model3.1)

simulationOutput_model3.2 <- simulateResiduals(fittedModel = m3.2_since_final) #create simulated data
testDispersion(simulationOutput_model3.2)

#Test for outliers
testOutliers(simulationOutput_model3.1)

testOutliers(simulationOutput_model3.2)

#Check for zero inflation
testZeroInflation(simulationOutput_model3.1)

testZeroInflation(simulationOutput_model3.2)



##Graph model 3.1


# Reshape intervention by years
sightings_long <- sightings %>%
  pivot_longer(
    cols = c(last3y_humanpop, last2y_humanpop, last1y_humanpop),
    names_to = "YearBreakdown",
    values_to = "sterilization_effort"
  ) %>%
  mutate(
    YearBreakdown = case_when(
      YearBreakdown == "last3y_humanpop" ~ 3,
      YearBreakdown == "last2y_humanpop" ~ 2,
      YearBreakdown == "last1y_humanpop" ~ 1
    )
  )

#Refit model
m3.1_final_long <- glmer(
  Adult.Lactating.female ~ sterilization_effort * YearBreakdown+ (1 | polygon),
  family = binomial,
  data = sightings_long,
  control = glmerControl(optimizer = "bobyqa")
)

#Predicted values for m3.1_final_long over "YearBreakdown"
preds3.1 <- ggpredict(m3.1_final_long, terms = c("YearBreakdown"))

# Plot
ggplot(preds3.1, aes(x = x, y = predicted)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  scale_x_reverse(breaks = c(3, 2, 1)) +  # 3 years ago on left
  labs(
    title = "Predicted Probability of Being a Lactating Female by Years",
    x = "Years Ago",
    y = "Probability of Being a Lactating Female",
    color = "Years Ago",
    fill = "Years Ago"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  scale_fill_viridis_d(option = "C", end = 0.9)

##Graph model 3.2

# Get predicted values over 'effort_3y_humanpop'
preds3.2 <- ggpredict(m3.2_year_final, terms = c("effort_3y_humanpop"))

# Plot
ggplot(preds3.2, aes(x = x, y = predicted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted Probability of Being a Puppy by\n Sterilization 3 Years Ago",
    x = "Sterilization Effort (Per Capita)",
    y = "Probability of Being a Puppy",) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  scale_fill_viridis_d(option = "C", end = 0.9)
