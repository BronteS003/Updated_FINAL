################################################################################
#            FULL Model 3: Presence of Lactating Females and Puppies           #
################################################################################
# Data from sight and resight surveys as recorded with FULL updated data set   #
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
library(car) #checking vif
library(scales) #axis and legend label appearances

################################################################################

##IMPORT DATA##

#Read rds for sightings file

sightings <- readRDS("FULL_sightings.rds", refhook = NULL)

################################################################################

#Investigate numbers of puppies & females
table(sightings$Puppy) #there's 531 adults/18 puppies

table(sightings$Adult.Lactating.female) # there's 540 non-lactating/9 lactating females

################################################################################

##CLEAN DATA - Lactating Females##

#Remove puppies as they're not relevant to analysis
pop_sightings <- sightings %>% 
  filter(Puppy != 1)

#Remove males
pop_sightings <- pop_sightings %>% 
  filter(Adult.male != 1)

################################################################################

##MODEL SELECTION (Sightings) - Lactating Females##

#Time since intervention

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

#Total Effort

#Most complex model, lactating females by total sterilization effort
m3.1_effort <- glmer(Adult.Lactating.female ~ effort_humanpop + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_effort)#all fine
drop1(m3.1_effort, test = "Chisq") 

#Create updated model dropping owned
m3.1_1effort <- glmer(Adult.Lactating.female ~ effort_humanpop + subdistrict +
                       (1 | polygon),
                     family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1effort, test = "Chisq") 

#Create updated model dropping effort
m3.1_2effort <- glmer(Adult.Lactating.female ~ subdistrict +
                        (1 | polygon),
                      family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2effort, test = "Chisq") #None of the variables are significant

################################################################################

#Effort by year

#Most complex model, lactating females by effort annually
m3.1_year <- glmer(Adult.Lactating.female ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict +
                       (1 | polygon),
                     family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_year) #drop effort 2y
m3.1_year <- glmer(Adult.Lactating.female ~ effort_4y_humanpop + effort_3y_humanpop + effort_1y_humanpop + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_year) #all now fine

#Check which variable to drop
drop1(m3.1_year, test = "Chisq") 

#Create updated model dropping 4y
m3.1_1year<- glmer(Adult.Lactating.female ~ effort_3y_humanpop + effort_1y_humanpop + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1year, test = "Chisq") 

#Create updated model dropping owned
m3.1_2year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop + effort_1y_humanpop + subdistrict +
                      (1 | polygon),
                    family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2year, test = "Chisq") 

#Create updated model dropping effort 1y ago
m3.1_3year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop + subdistrict +
                      (1 | polygon),
                    family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_3year, test = "Chisq")

#Create updated model dropping effort 1y ago
m3.1_final_year <- glmer(Adult.Lactating.female ~ effort_3y_humanpop +
                      (1 | polygon),
                    family = binomial, data = pop_sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_final_year, test = "Chisq") #Slightly significant

################################################################################
################################################################################

##MODEL SELECTION (SUMMARY) - LACTATING FEMALES##

#Read rds for dog density file

summary_data <- readRDS("FULL_dog_density.rds", refhook = NULL)

##CLEAN DATA##
#Remove puppies as they're not relevant to analysis
summary_data$Puppy <- NULL

#Remove males
summary_data$Adult.male <- NULL

#Make owned a proportion
summary_data$prop_owned <- summary_data$Owned / 
  (summary_data$Owned + summary_data$Free.roaming.NO.collar)




##MODEL SELECTION (Sightings) - Lactating Females##

#Time since intervention

#Most complex model, lactating females by time since intervention
m3.1_since <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ since_intervention + prop_owned + subdistrict +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_since)#all fine
drop1(m3.1_since, test = "Chisq") 

#Drop subdistrict
m3.1_1since <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ since_intervention + prop_owned +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1since, test = "Chisq") 

#Drop since_intervention
m3.1_2since <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ prop_owned +
                       (1 | polygon/survey),
                     family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2since, test = "Chisq") #None significant




#Total Effort

#Most complex model, lactating females by total sterilization effort
m3.1_effort <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ effort_humanpop + prop_owned + subdistrict +
                       (1 | polygon/survey),
                     family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_effort)#all fine
drop1(m3.1_effort, test = "Chisq") 

#Create updated model dropping subdistrict
m3.1_1effort <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ effort_humanpop + prop_owned +
                        (1 | polygon/survey),
                      family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1effort, test = "Chisq") 

#Create updated model dropping effort
m3.1_2effort <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ prop_owned +
                        (1 | polygon/survey),
                      family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2effort, test = "Chisq") ##None significant




#Effort by year

#Most complex model, lactating females by effort annually
m3.1_year <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict + prop_owned +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.1_year) #drop 1y
#Drop year 1
m3.1_year <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + subdistrict + prop_owned +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))

#Check which variable to drop
drop1(m3.1_year, test = "Chisq") 

#Create updated model dropping 4y
m3.1_1year <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ effort_3y_humanpop + effort_2y_humanpop + subdistrict + prop_owned +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_1year, test = "Chisq") 

#Create updated model dropping owned
m3.1_2year <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ effort_3y_humanpop + effort_2y_humanpop + subdistrict + 
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_2year, test = "Chisq") 

#Create updated model dropping subdistrict
m3.1_3year <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ effort_3y_humanpop + effort_2y_humanpop +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_3year, test = "Chisq") 

#Create updated model dropping 2y
m3.1_4year <- glmer(cbind(Adult.Lactating.female,Adult.NON.lactating.female) ~ effort_3y_humanpop + 
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.1_4year, test = "Chisq")#Significant

################################################################################

##MODEL SELECTION (sightings) - Puppies##

#Time since intervention

#Most complex model, puppies by time since intervention
m3.2_since <- glmer(Puppy ~ since_intervention + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m3.2_since)
vif(m3.2_since)
drop1(m3.2_since, test = "Chisq")

#Updated model dropping owned
m3_final_since <- glmer(Puppy ~ since_intervention + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3_final_since, test = "Chisq") #Significant

################################################################################

#Total Effort

#Most complex model, puppies by total sterilization effort
m3.2_effort <- glmer(Puppy ~ effort_humanpop + owned + subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m3.2_effort)
vif(m3.2_effort)
drop1(m3.2_effort, test = "Chisq")

#Drop owned
m3.2_1effort <- glmer(Puppy ~ effort_humanpop + subdistrict +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_1effort, test = "Chisq")

#Drop subdistrict
m3_final_effort <- glmer(Puppy ~ effort_humanpop +
                        (1 | polygon),
                      family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3_final_effort, test = "Chisq")# Significant

################################################################################

#Effort by year

#Most complex model, puppy by effort annually
m3.2_year <- glmer(Puppy ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m3.2_year)
vif(m3.2_year) #drop effort 2y
m3.2_year <- glmer(Puppy ~ effort_4y_humanpop + effort_3y_humanpop + effort_1y_humanpop + owned + subdistrict +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m3.2_year) #all good
drop1(m3.2_year, test = "Chisq") 

#Updated model dropping 4y
m3.2_1year <- glmer(Puppy ~ effort_3y_humanpop + effort_1y_humanpop + owned + subdistrict +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_1year, test = "Chisq") 

#Updated model dropping 1y
m3.2_2year  <- glmer(Puppy ~ effort_3y_humanpop + owned +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_2year, test = "Chisq")

#Updated model dropping owned
m3_final_year  <- glmer(Puppy ~ effort_3y_humanpop +
                       (1 | polygon),
                     family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m3_final_year, test = "Chisq")#Significant

#Compare models
AIC(m3_final_since, m3_final_effort, m3_final_year) #year is the best fit

###############################################################################
###############################################################################

##MODEL SELECTION (SUMMARY) - PUPPIES##
#Read rds for sightings file

summary_data <- readRDS("FULL_dog_density.rds", refhook = NULL)

# Add column for adult
summary_data <- summary_data %>%
  mutate(adult = rowSums(across(c(Adult.male, Adult.NON.lactating.female, Adult.Lactating.female)), na.rm = TRUE))

#Make owned a proportion
summary_data$prop_owned <- summary_data$Owned / 
  (summary_data$Owned + summary_data$Free.roaming.NO.collar)

##Time Since Intervention##

#Most complex model
m3.2_since <- glmer(cbind(Puppy,adult) ~ since_intervention + prop_owned + subdistrict +
           (1 | polygon/survey),
         family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.2_since)#all good
drop1(m3.2_since, test = "Chisq")

#Updated model dropping owned
m3.2_since <- glmer(cbind(Puppy,adult) ~ since_intervention + subdistrict +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_since, test = "Chisq")

#Updated model dropping subdistrict
m3.2_since <- glmer(cbind(Puppy,adult) ~ since_intervention +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_since, test = "Chisq")

#Final model for since intervention
m3.2_final_since <- glmer(cbind(Puppy,adult) ~ since_intervention +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))

##Total Effort##

#Most complex model
m3.2_total <- glmer(cbind(Puppy,adult) ~ effort_humanpop + prop_owned + subdistrict +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.2_total)#all good
drop1(m3.2_total, test = "Chisq")

#Creat updated model, dropping owned
m3.2_total <- glmer(cbind(Puppy,adult) ~ effort_humanpop + subdistrict +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_total, test = "Chisq")

#Final model for total effort
m3.2_final_total <- glmer(cbind(Puppy,adult) ~ effort_humanpop + subdistrict +
                            (1 | polygon/survey),
                          family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))

###Effort by Year ##

#Most complex model
m3.2_year <- glmer(cbind(Puppy,adult) ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + prop_owned + subdistrict +
                      (1 | polygon/survey),
                    family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
vif(m3.2_year)#drop 2y
m3.2_year <- glmer(cbind(Puppy,adult) ~ effort_3y_humanpop + effort_1y_humanpop + prop_owned + subdistrict +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_year, test = "Chisq")

#Create updated model dropping owned
m3.2_year <- glmer(cbind(Puppy,adult) ~ effort_3y_humanpop + effort_1y_humanpop + subdistrict +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_year, test = "Chisq")

#Create updated model, dropping 1y
m3.2_year <- glmer(cbind(Puppy,adult) ~ effort_3y_humanpop + subdistrict +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_year, test = "Chisq")

#Create updated model, dropping subdistrict
m3.2_year <- glmer(cbind(Puppy,adult) ~ effort_3y_humanpop +
                     (1 | polygon/survey),
                   family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))
drop1(m3.2_year, test = "Chisq")

#Final model
m3.2_final_year <- glmer(cbind(Puppy,adult) ~ effort_3y_humanpop +
                           (1 | polygon/survey),
                         family = binomial, data = summary_data, control = glmerControl(optimizer = "bobyqa"))

##Compare the fit of 3 final models
AIC(m3.2_final_since,m3.2_final_total, m3.2_final_year) #smallest AIC is final since

################################################################################

##Check for overdispersion##

#Check residual deviance relative to degrees of freedom for model3.2
overdisp.glmer(m3.2_final_since)

#Visually check overdispersion using DHARMa plot
simulationOutput_model3.2 <- simulateResiduals(fittedModel = m3.2_final_since) #create simulated data
testDispersion(simulationOutput_model3.2)

simulationOutput_model3.2 <- simulateResiduals(fittedModel = m3.2_final_since) #create simulated data
testDispersion(simulationOutput_model3.2)

#Test for outliers
testOutliers(simulationOutput_model3.2)

#Check for zero inflation
testZeroInflation(simulationOutput_model3.2)

################################################################################

##Graph model 3.2 - Puppies##

#Get predicted values over time since intervention
preds_since <- ggpredict(m3.2_final_since, terms = "since_intervention", type = "fixed")

plot(preds_since) + 
  labs(title = "Probability of Being a Puppy by\n Time Since Intervention\n and Subdistrict",
       x = "Time Since Intervention (Years)",
       y = "Probability of Being Puppy") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 0.2)) 


# Get predicted values over year
preds_puppies_3y <- ggpredict(m3.2_final_year, terms = "effort_3y_humanpop", type = "fixed")

plot(preds_puppies_3y) +
  labs(
    title = "Predicted Probability of Observing a Puppy\n by Sterilization Effort 3 Years Ago",
    x = "Sterilizations per Human Capita (3 Years Ago)",
    y = "Probability of Being a Puppy"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  coord_cartesian(ylim = c(0, 0.2)) 

