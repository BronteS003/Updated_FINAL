################################################################################
#             FULL Model 2: Change in Sterilization Status Overtime            #
################################################################################
# Data from sight and resight surveys, updated data (2021-2025)                #
################################################################################
# Created Oct 15, 2025 by Bronte Slote, last modified Apr. 27, 2026            #
################################################################################

##Load Libraries
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
library(emmeans)
library(patchwork)
library(tidyr)
library(scales)

################################################################################

##LOAD DATA##

#load RDS file
sightings <- readRDS("Data/sightings_v2.rds")

################################################################################

##CLEAN DATA##

#Remove puppies as they're not relevant to analysis (cannot be sterilized)
sightings <- sightings %>% 
  filter(Puppy != 1)

#Remove adults with unknown sterilization status 
sightings <- sightings %>% 
  filter(Unknown != 1)

################################################################################

##FIT MODEL - SINCE INTERVENTION##

#Most complex model
m2_since <- glmer(Neutered ~ since_intervention + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2_since)

#Check vif
vif(m2_since)

#Drop 1 test
drop1(m2_since, test = "Chisq") #drop subdistrict

#updated model 2 dropping subdistrict
m2.1_since <- glmer(Neutered ~ since_intervention + owned + sex +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.1_since)

#Drop 1 test
drop1(m2.1_since, test = "Chisq") 

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.1_since) #good 1.037

#Check with DHARMa
simulationOutput_m2_since <- simulateResiduals(fittedModel = m2.1_since) #create simulated data

testDispersion(simulationOutput_m2_since)

testOutliers(simulationOutput_m2_since)

testZeroInflation(simulationOutput_m2_since)

testUniformity(simulationOutput_m2_since)

plot(simulationOutput_m2_since)

################################################################################

##FIT MODEL - TOTAL STERILIZATION EFFORT##

#Most complex model
m2_total <- glmer(Neutered ~ effort_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2_total)

#check vif
vif(m2_total)

#Drop 1 test
drop1(m2_total, test = "Chisq") #drop subdistrict

#updated model 2 dropping subdistrict
m2.1_total <- glmer(Neutered ~ effort_humanpop + owned + sex +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.1_total)

#Drop 1 test
drop1(m2.1_total, test = "Chisq") #drop owned

#updated model 2 dropping owned
m2.2_total <- glmer(Neutered ~ effort_humanpop + sex +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.2_total)

#Drop 1 test
drop1(m2.2_total, test = "Chisq") #all significant

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.2_total) #good 0.989

#Check with DHARMa
simulationOutput_m2_total <- simulateResiduals(fittedModel = m2.2_total) #create simulated data

testDispersion(simulationOutput_m2_total)

testOutliers(simulationOutput_m2_total)

testZeroInflation(simulationOutput_m2_total)

testUniformity(simulationOutput_m2_total)

plot(simulationOutput_m2_total) #problem with quantile deviation?

################################################################################

##FIT MODEL - YEARLY STERILIZATION EFFORT##

#Most complex model
m2_year <- glmer(Neutered ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2_year)

#check vif
vif(m2_year)

#Drop 1 test
drop1(m2_year, test = "Chisq") #drop 4 years ago

#updated model 2 dropping 4 years ago
m2.1_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.1_year)

#Drop 1 test
drop1(m2.1_year, test = "Chisq") #drop subdistrict

#updated model 2 dropping owned
m2.2_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + sex +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.2_year)

#Drop 1 test
drop1(m2.2_year, test = "Chisq") #drop owned

#updated model 2 dropping owned
m2.3_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + sex +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.3_year)

#Drop 1 test
drop1(m2.3_year, test = "Chisq") #all good

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.2_year) #good 0.966

#Check with DHARMa
simulationOutput_m2_year <- simulateResiduals(fittedModel = m2.2_year) #create simulated data

testDispersion(simulationOutput_m2_year)

testOutliers(simulationOutput_m2_year)

testZeroInflation(simulationOutput_m2_year)

testUniformity(simulationOutput_m2_year)

plot(simulationOutput_m2_year)

################################################################################

##MODEL COMPARISON##

#Compare models with AIC
AIC(m2.1_since, m2.2_total, m2.3_year) #year is the best fit with the lowest AIC (504.8550), then total (475.9618) and since (504.8525)

################################################################################

##PLOT MODELS - SINCE INTERVENTION##

#get predicted values for since intervention model and sex
preds_since <- ggpredict(m2.1_since, terms = c("since_intervention", "sex"))

#plot since_intervention model
plot(preds_since) +
  labs(
    title = "Predicted Probability of Being Neutered\n by Time Since Intervention",
    x = "Time Since Intervention (Years)",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#get predicted values for ownership status
preds_owned <- ggpredict(m2.1_since, terms = c("owned"))

#plot sterilization by ownership status
ggplot(preds_owned, aes(x = x, y = predicted)) +
  geom_col() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width =
                  0.2) +
  labs(title = "Probability of being Neutered by\n Ownership Status",
       x = "Ownership Status",
       y = "Probability of Being Neutered") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank())

#get predicted values for sex
preds_sex <- ggpredict(m2.1_since, terms = c("sex"))

#plot sterilization by sex
ggplot(preds_sex, aes(x = x, y = predicted)) +
  geom_col() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width =
                  0.2) +
  labs(title = "Probability of being Neutered by\n Sex",
       x = "Sex",
       y = "Probability of Being Neutered") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank())

################################################################################

##PLOT MODELS - TOTAL EFFORT##

#get predictions over total effort sex
preds_total <- ggpredict(m2.2_total, terms = c("effort_humanpop","sex"))

#plot total effort model
ggplot(preds_total, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(title = "Probability of being Neutered by\n Total Sterilization Effort\n and Sex",
       x = "Total Dog Sterilizations Conducted (Per Human Capita)",
       y = "Probability of Being Neutered") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  )

################################################################################

##PLOT MODELS - YEARLY EFFORT##

#Create predicted values for 3 years ago
preds_3y <- ggpredict(m2.2_year, terms = "effort_3y_humanpop", type = "fixed")

plot(preds_3y) +
  labs(
    title = "Predicted Probability of Being Neutered\nvs Sterilization Effort 3 Years Ago",
    x = "Total Sterilizations Conducted 3 Years Ago (Per Human Capita)",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Create predicted values for 2 years ago
preds_2y <- ggpredict(m2.2_year, terms = "effort_2y_humanpop", type = "fixed")

plot(preds_2y) +
  labs(
    title = "Predicted Probability of Being Neutered\nvs Sterilization Effort 2 Years Ago",
    x = "Total Sterilizations 2 years ago (Per Human Capita)",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Create predicted values for 1 years ago
preds_1y <- ggpredict(m2.2_year, terms = "effort_1y_humanpop", type = "fixed")

plot(preds_1y) +
  labs(
    title = "Predicted Probability of Being Neutered\nvs Sterilization Effort 1 Year Ago",
    x = "Total Sterilizations 1 year ago (Per Human Capita)",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Put it all in the same plot
preds_3y$Outcome <- "3 Years Ago"
preds_2y$Outcome <- "2 Years Ago"
preds_1y$Outcome <- "1 Year Ago"
preds_all <- rbind(preds_3y, preds_2y,preds_1y)

sterilization_change <- ggplot(preds_all, aes(x = x, y = predicted, color = Outcome, fill = Outcome)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  labs(
    x = "Number of Sterilizations (Per Human Capita)",
    y = "Probability of being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_color_manual(values = c("3 Years Ago" = "#2E86AB", "2 Years Ago" = "#E07A5F", "1 Year Ago" = "darkgreen")) +
  scale_fill_manual(values = c("3 Years Ago" = "#2E86AB", "2 Years Ago" = "#E07A5F", "1 Year Ago" = "darkgreen"))

#save plot
ggsave("Plots/Model2_Sterilization_Effort_Change.png", plot = sterilization_change, width = 8, height = 6, dpi = 300)

################################################################################


