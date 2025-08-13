################################################################################
#             Model 1: Change in Dog Density Over Time                         #
################################################################################
# Data from sight and resight surveys as recorded in WVS, with track lengths   #
# from Talea to calculate dogs per km of track surveyed                        #
################################################################################
# Created June 3, 2025 by Bronte Slote, last modified JuLY 17, 2025             #
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
library(lmtest) # for likelihood ratio tests
library(car) # for variance inflation factors
library(corrplot) # to check for correlation between EVs
library(patchwork) #combine graphs into 1 panel


##IMPORT DATA SET##

#Read rds for dog_density file

dog_density <- readRDS("dog_density.rds", refhook = NULL)

##VISUALIZING DATA##

#Create jitter plot with line of best fit showing dogs per km by date and subdistrict
ggplot(dog_density,aes(x=since_intervention,y=Sighting.Count,colour=subdistrict))+
  geom_jitter()+
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Jitter Plot with Line of Best Fit") +
  theme_minimal()

#Create jitter plot with line of best fit showing dogs per km by date and polygon
ggplot(dog_density,aes(x=since_intervention,y=Sighting.Count,colour=polygon))+
  geom_jitter()+
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Jitter Plot with Line of Best Fit") +
  theme_minimal()

##MODEL SELECTION##

#Time Since Intervention

#Create m1 using time since intervention
m1_since_intervention <- glmer(Sighting.Count ~ since_intervention + Track.Length + subdistrict + day + Mode.Transport +
                (1 | polygon/survey) +
                        offset(log(Track.Length)), 
                      family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

# Check variance inflation factors
vif(m1_since_intervention) # all fine

#Test what variables should be dropped
drop1(m1_since_intervention, test="Chisq")

#Create updated m1, dropping day 
m1_1.since_intervention <- glmer(Sighting.Count ~ since_intervention + Track.Length + subdistrict + Mode.Transport +
                                 (1 | polygon/survey) +
                                 offset(log(Track.Length)), 
                               family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

#Test what variables should be dropped
drop1(m1_1.since_intervention, test="Chisq")

#Create updated m1, dropping mode.transport
m1_2.since_intervention <- glmer(Sighting.Count ~ since_intervention + subdistrict + Track.Length +
                                   (1 | polygon/survey) +
                                   offset(log(Track.Length)), 
                                 family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

#Test what variables should be dropped
drop1(m1_2.since_intervention, test="Chisq")

# Dropping Track.Length
final_since_intervention <- glmer(Sighting.Count ~ since_intervention + subdistrict +
                                    (1 | polygon/survey) +
                                    offset(log(Track.Length)), 
                                  family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

##All remaining variables are significant
drop1(final_since_intervention, test="Chisq")


#Total Sterilization Effort

#Create m1 using total sterilization effort by human population
m1_effort_humanpop <- glmer(Sighting.Count ~ effort_humanpop + Track.Length + subdistrict + day + Mode.Transport +
                                 (1 | polygon/survey) +
                                 offset(log(Track.Length)), 
                               family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

# Check variance inflation factors
vif(m1_effort_humanpop) # all fine

#Test what variables should be dropped
drop1(m1_effort_humanpop, test="Chisq")

#Create updated m1, dropping day
m1_1.effort_humanpop <- glmer(Sighting.Count ~ effort_humanpop + Track.Length + subdistrict + Mode.Transport +
                              (1 | polygon/survey) +
                              offset(log(Track.Length)), 
                            family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_1.effort_humanpop, test="Chisq")

#Create updated m1, dropping day
m1_2.effort_humanpop <- glmer(Sighting.Count ~ effort_humanpop + Track.Length + subdistrict + Mode.Transport +
                                (1 | polygon/survey) +
                                offset(log(Track.Length)), 
                              family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_2.effort_humanpop, test="Chisq")

#Create updated m1, dropping mode.transport
m1_3.effort_humanpop <- glmer(Sighting.Count ~ effort_humanpop + Track.Length + subdistrict +
                                (1 | polygon/survey) +
                                offset(log(Track.Length)), 
                              family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_3.effort_humanpop, test="Chisq")

#Create updated m1, dropping track length
m1_4.effort_humanpop <- glmer(Sighting.Count ~ effort_humanpop + subdistrict +
                                (1 | polygon/survey) +
                                offset(log(Track.Length)), 
                              family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_4.effort_humanpop, test="Chisq")

#Final model for effort
m1_effort_final <- glmer(Sighting.Count ~ effort_humanpop + subdistrict +
                                (1 | polygon/survey) +
                                offset(log(Track.Length)), 
                              family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))


##Sterilization by Year##

#Most complex m1 using years
m1_year <- glmer(Sighting.Count ~ three_years_ago_humanpop + two_years_ago_humanpop + last1y_humanpop + Track.Length + subdistrict + day + Mode.Transport +
                              (1 | polygon/survey) +
                              offset(log(Track.Length)), 
                            family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

# Check variance inflation factors
vif(m1_year) # problems with the effort variables - all have VIF>5

# Remove variable with the highest VIF
m1_year <- glmer(Sighting.Count ~ three_years_ago_humanpop + last1y_humanpop + Track.Length + subdistrict + day + Mode.Transport +
                   (1 | polygon/survey) +
                   offset(log(Track.Length)), 
                 family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

# Check variance inflation factors
vif(m1_year) # all ok now - can move on to backward selection using drop 1

#Test what variables should be dropped
drop1(m1_year, test="Chisq")

#Create updated m1, dropping year 2
m1_1year <- glmer(Sighting.Count ~ last3y_humanpop + last1y_humanpop + Track.Length + subdistrict + day + Mode.Transport +
                   (1 | polygon/survey) +
                   offset(log(Track.Length)), 
                 family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_1year, test="Chisq")

#Create updated m1, dropping year 1
m1_2year <- glmer(Sighting.Count ~ last3y_humanpop + Track.Length + subdistrict + day + Mode.Transport +
                    (1 | polygon/survey) +
                    offset(log(Track.Length)), 
                  family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_2year, test="Chisq")

#Create updated m1, dropping day
m1_3year <- glmer(Sighting.Count ~ last3y_humanpop + Track.Length + subdistrict + Mode.Transport +
                    (1 | polygon/survey) +
                    offset(log(Track.Length)), 
                  family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_3year, test="Chisq")

#Final year model
m1_year_final <- glmer(Sighting.Count ~ last3y_humanpop + Track.Length + subdistrict + Mode.Transport +
                    (1 | polygon/survey) +
                    offset(log(Track.Length)), 
                  family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))


#Compare the three models
AIC(final_since_intervention, m1_effort_final, m1_year_final)


##Final model 1##


#Final model with since intervention
m1_final_since<- glmer(Sighting.Count ~ since_intervention + subdistrict +
           (1 | polygon/survey) +
           offset(log(Track.Length)), 
         family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

#Final model with effort
m1_final_effort <- glmer(Sighting.Count ~ effort_humanpop + subdistrict +
                                           (1 | polygon/survey) +
                                           offset(log(Track.Length)), 
                                         family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))


##CHECK FOR OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
  overdisp.glmer(m1_final_effort)
overdisp.glmer(m1_final_since)

#Visually check overdispersion using DHARMa plot
simulationOutput_m1_effort <- simulateResiduals(fittedModel = m1_final_effort) #create simulated data
testDispersion(simulationOutput_m1_effort)

simulationOutput_m1_since <- simulateResiduals(fittedModel = m1_final_since) #create simulated data
testDispersion(simulationOutput_m1_since)

#Test for outliers
testOutliers(simulationOutput_m1_effort)
testOutliers(simulationOutput_m1_since)

#Check for zero inflation
testZeroInflation(simulationOutput_m1_effort)
testZeroInflation(simulationOutput_m1_since)

# Residual plots
plot(simulationOutput_m1_effort)
plot(simulationOutput_m1_since) #does this show a problem with DHARMa residuals


##PLOTTING MODELS##

# Get predicted values over all-time effort
preds1 <- ggpredict(m1_final_effort, terms = c("effort_humanpop", "subdistrict"),
                    condition = c(Track.Length=1))

p1 <-ggplot(preds1, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm") +
  labs(title = "Predicted Sightings/km by\n All Time Sterilization Effort\n and Subdistrict",
       x = "All Time Effort (per Capita)",
       y = "Predicted Sightings per km",
       colour = "Subdistrict") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  scale_fill_viridis_d(option = "C", end = 0.9)


# Get predicted values over time since intervention
preds1.1 <- ggpredict(m1_final_since, terms = c("since_intervention", "subdistrict"),
                    condition = c(Track.Length=1))

p2 <- ggplot(preds1.1, aes(x = x, y = predicted, colour = group)) +
  stat_smooth(method = "lm") +
  labs(title = "Predicted Sightings/km by\n Years Since Intervention\n and Subdistrict",
       x = "Time Since Intervention (Year)",
       y = "Predicted Sightings per km",
       colour = "Subdistrict") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  scale_fill_viridis_d(option = "C", end = 0.9)

#Combine into one panel with a shared legend
p1 + p2 + plot_layout(guides = "collect")


