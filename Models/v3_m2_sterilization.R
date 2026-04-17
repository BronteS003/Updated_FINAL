################################################################################
##            Model 2: Change in Sterilization Status                         ##
################################################################################
# Model testing effects of sterilization effort on the probability that a dog  #
# is sterilized. Using updated RDS file.                                        #
################################################################################
# Created April 16, 2026, by Bronte Slote, last edited April 16, 2026          #
################################################################################

##LOAD LIBRARIES##
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(lme4) #fitting linear mixed models
library(car) #checking multicollinearity

################################################################################

##LOAD DATA##

#load RDS file
sightings <- readRDS("Data/sightings_v3.rds")

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
m2_total <- glmer(Neutered ~ sc_total + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2_total)

#check vif
vif(m2_total)

#Drop 1 test
drop1(m2_total, test = "Chisq") #drop subdistrict

#updated model 2 dropping subdistrict
m2.1_total <- glmer(Neutered ~ sc_total + owned + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.1_total)

#Drop 1 test
drop1(m2.1_total, test = "Chisq") #drop owned

#updated model 2 dropping owned
m2.2_total <- glmer(Neutered ~ sc_total + sex +
                      (1 | polygon),
                    family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.2_total)

#Drop 1 test
drop1(m2.2_total, test = "Chisq") #all significant

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m2.2_total) #good 0.968

#Check with DHARMa
simulationOutput_m2_total <- simulateResiduals(fittedModel = m2.2_total) #create simulated data

testDispersion(simulationOutput_m2_total)

testOutliers(simulationOutput_m2_total)

testZeroInflation(simulationOutput_m2_total)

testUniformity(simulationOutput_m2_total)

plot(simulationOutput_m2_total)

################################################################################

##FIT MODEL - YEARLY STERILIZATION EFFORT##

#Most complex model
m2_year <- glmer(Neutered ~ sc_4y + sc_3y + sc_2y + sc_1y + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2_year)

#check vif
vif(m2_year)

#Drop 1 test
drop1(m2_year, test = "Chisq") #drop 4 years ago

#updated model 2 dropping 4 years ago
m2.1_year <- glmer(Neutered ~ sc_3y + sc_2y + sc_1y + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.1_year)

#Drop 1 test
drop1(m2.1_year, test = "Chisq") #drop subdistrict

#updated model 2 dropping owned
m2.2_year <- glmer(Neutered ~ sc_3y + sc_2y + sc_1y + owned + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.2_year)

#Drop 1 test
drop1(m2.2_year, test = "Chisq") #drop owned

#updated model 2 dropping owned
m2.3_year <- glmer(Neutered ~ sc_3y + sc_2y + sc_1y + sex +
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
AIC(m2.1_since, m2.2_total, m2.3_year) #year is the best fit with the lowest AIC (504.8550), then total (477.8754) and since (504.8550)

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
preds_total <- ggpredict(m2.2_total, terms = c("sc_total","sex"))

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
preds_3y <- ggpredict(m2.2_year, terms = "sc_3y", type = "fixed")

plot(preds_3y) +
  labs(
    title = "Predicted Probability of Being Neutered\nvs Sterilization Effort 3 Years Ago",
    x = "Total Sterilizations Conducted 3 Years Ago (Per Human Capita)",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Create predicted values for 2 years ago
preds_2y <- ggpredict(m2.2_year, terms = "sc_2y", type = "fixed")

plot(preds_2y) +
  labs(
    title = "Predicted Probability of Being Neutered\nvs Sterilization Effort 2 Years Ago",
    x = "Total Sterilizations 2 years ago (Per Human Capita)",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Create predicted values for 1 years ago
preds_1y <- ggpredict(m2.2_year, terms = "sc_1y", type = "fixed")

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

ggplot(preds_all, aes(x = x, y = predicted, color = Outcome, fill = Outcome)) +
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
