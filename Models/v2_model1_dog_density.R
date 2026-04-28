################################################################################
#             FULL Model 1: Change in Dog Density Over Time                    #
################################################################################
# Data from sight and resight surveys as recorded in WVS, with track lengths   #
# from Talea to calculate dogs per km of track surveyed. Using full dataset    #
# (2021-2025).                                                                 #
################################################################################
# Created Oct. 15, 2025 by Bronte Slote, last modified Apr. 24, 2026           #
################################################################################

##Load Libraries##
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
library(corrplot) # to check for correlation between explanatory variables
library(patchwork) #combine graphs into 1 panel

################################################################################

##IMPORT DATA SET##

#Read rds for dog_density file
dog_density <- readRDS("Data/density_v2.rds", refhook = NULL)

################################################################################

##FIT MODEL - TIME SINCE INTERVENTION##

#Most complex model 
m1_since_intervention <- glmer(Sighting.Count ~ since_intervention + subdistrict + day + Mode.Transport +
                (1 | polygon) +
                        offset(log(Track.Length)), 
                      family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1_since_intervention)

# Check variance inflation factors
vif(m1_since_intervention) # all fine

#Test what variables should be dropped
drop1(m1_since_intervention, test="Chisq")

#Create updated m1, dropping day 
m1_1.since_intervention <- glmer(Sighting.Count ~ since_intervention + subdistrict + Mode.Transport +
                                 (1 | polygon) +
                                 offset(log(Track.Length)), 
                               family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

#Test what variables should be dropped
drop1(m1_1.since_intervention, test="Chisq")

#Create updated m1, dropping mode.transport
final_since_intervention <- glmer(Sighting.Count ~ since_intervention + subdistrict +
                                   (1 | polygon) +
                                   offset(log(Track.Length)), 
                                 family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

#Test what variables should be dropped
drop1(final_since_intervention, test="Chisq")# all remaining variables significant

##CHECK MODEL FIT##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(final_since_intervention) #1.347, is this close to concerning(?)

#Check with DHARMa
simulationOutput_m1_since <- simulateResiduals(fittedModel = final_since_intervention) #create simulated data

testDispersion(simulationOutput_m1_since)

testOutliers(simulationOutput_m1_since)

testZeroInflation(simulationOutput_m1_since)

testUniformity(simulationOutput_m1_since)

plot(simulationOutput_m1_since) #is there a problem with quantile deviation?

################################################################################

##FIT MODEL - TOTAL STERILIZATION EFFORT##

#Create m1 using total sterilization effort by human population
m1_effort_humanpop <- glmer(Sighting.Count ~ effort_humanpop + subdistrict + day + Mode.Transport +
                                 (1 | polygon) +
                                 offset(log(Track.Length)), 
                               family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1_effort_humanpop)

# Check variance inflation factors
vif(m1_effort_humanpop) # all fine

#Test what variables should be dropped
drop1(m1_effort_humanpop, test="Chisq")

#Create updated m1, dropping day
m1_1.effort_humanpop <- glmer(Sighting.Count ~ effort_humanpop + subdistrict + Mode.Transport +
                              (1 | polygon) +
                              offset(log(Track.Length)), 
                            family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_1.effort_humanpop, test="Chisq")

#Create updated m1, dropping mode.transport
m1_effort_final <- glmer(Sighting.Count ~ effort_humanpop + subdistrict +
                                (1 | polygon) +
                                offset(log(Track.Length)), 
                              family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_effort_final, test="Chisq") #all variables significant

##CHECK MODEL FIT##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1_effort_final) #1.38, is this close to concerning(?)

#Check with DHARMa
simulationOutput_m1_effort <- simulateResiduals(fittedModel = m1_effort_final) #create simulated data

testDispersion(simulationOutput_m1_effort)

testOutliers(simulationOutput_m1_effort)

testZeroInflation(simulationOutput_m1_effort)

testUniformity(simulationOutput_m1_effort)

plot(simulationOutput_m1_effort) #is there a problem with quantile deviation?

##################################################################################################

##Sterilization by Year##

#Check for correlation between year variables
years_effort <- dog_density %>% 
  select(effort_4y_humanpop, effort_3y_humanpop,
         effort_2y_humanpop, effort_1y_humanpop)

corrplot(cor(years_effort, use = "pairwise.complete.obs"),
         method = "color",
         type = "upper",
         tl.col = "black") #No concerning correlation between years

#Most complex m1 using years
m1_year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict + day + Mode.Transport +
                              (1 | polygon) +
                              offset(log(Track.Length)), 
                            family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

# Check variance inflation factors
vif(m1_year) # all good

#Test what variables should be dropped
drop1(m1_year, test="Chisq")

#Create updated m1, dropping year 2
m1_1year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_1y_humanpop + subdistrict + day + Mode.Transport +
                   (1 | polygon) +
                   offset(log(Track.Length)), 
                 family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_1year, test="Chisq")

#Create updated m1, dropping day
m1_2year <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_1y_humanpop + subdistrict + Mode.Transport +
                       (1 | polygon) +
                       offset(log(Track.Length)), 
                     family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_2year, test="Chisq")

#Create updated m1, dropping mode of transport
m1_year_final <- glmer(Sighting.Count ~ effort_4y_humanpop + effort_3y_humanpop + effort_1y_humanpop + subdistrict +
                    (1 | polygon) +
                    offset(log(Track.Length)), 
                  family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
#Test what variables should be dropped
drop1(m1_year_final, test="Chisq") #all remaining variables significant

##CHECK MODEL FIT##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1_year_final) #1.356, is this close to concerning(?)

#Check with DHARMa
simulationOutput_m1_year <- simulateResiduals(fittedModel = m1_year_final) #create simulated data

testDispersion(simulationOutput_m1_year)

testOutliers(simulationOutput_m1_year)

testZeroInflation(simulationOutput_m1_year)

testUniformity(simulationOutput_m1_year)

plot(simulationOutput_m1_year) #is there a problem with quantile deviation?

################################################################################

#Compare the three models
AIC(final_since_intervention, m1_effort_final, m1_year_final) # since intervention has lowest AIC, but they're all close

################################################################################

##Final model 1##


#Final model with since intervention
m1_final_since<- glmer(Sighting.Count ~ since_intervention + subdistrict +
                         (1 | polygon) +
                         offset(log(Track.Length)), 
                       family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

################################################################################

##PLOTTING MODELS##

# Get predicted values over time since intervention
preds1_since <- ggpredict(m1_final_since,
  terms = c("since_intervention", "subdistrict"),
  condition = c(Track.Length = 1),
  type = "fixed"
)

#Create raw data points for graphs - summarizing the average number of dogs sight per
# km per survey
raw_avg <- dog_density %>%
  group_by(survey) %>%
  summarise(
    mean_density = mean(Dogs.per.km, na.rm = TRUE),
    since_intervention = first(since_intervention),   # keep variables for plotting
    subdistrict = first(subdistrict),
    .groups = "drop"
  )
 
#Plot
plot(preds1_since) +
  geom_point(
    data = raw_avg,
    aes(x = since_intervention, y = mean_density, color = subdistrict),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = "Predicted Sightings/km by\n Time Since Intervention\n and Subdistrict",
       x = "Time Since Intervention (Years)",
       y = "Predicted Sightings per Km") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 22))


################################################################################
  
#Get predicted values for number of sightings at 0 years, 1 year, 2 year, 3 year
pred_exact <- ggpredict(
  m1_final_since,
  terms = c("since_intervention [0,1,2,3]", "subdistrict"),
  condition = c(Track.Length = 1)
)

################################################################################