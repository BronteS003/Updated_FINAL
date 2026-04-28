################################################################################
##                        Model 1: Dog Density                                ##
################################################################################
# Model testing the effects of sterilization effort on dog density. Using      #
# updated RDS file with no resights. V3 removes all resights and uses scale()  #
################################################################################
# Created April 15, 2026 by Bronte Slote, last edited April 16, 2026           #
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
library(lmtest) # for likelihood ratio tests
library(car) # for variance inflation factors
library(corrplot) # to check for correlation between explanatory variables
library(patchwork) #combine graphs into 1 panel

################################################################################

##LOAD RDS##
dog_density <- readRDS("Data/density_v3.rds")

################################################################################

##FIT MODEL - TIME SINCE INTERVENTION##

#Most complex model
m1_since <- glmer(Sighting.Count ~ since_intervention + subdistrict + day + Mode.Transport +
                                 (1 | polygon) +
                                 offset(log(Track.Length)), 
                               family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1_since)

#check VIF
vif(m1_since) #all good

#drop1 test
drop1(m1_since, test = "Chisq") #drop day

#Updated model 1 dropping day
m1.1_since <- glmer(Sighting.Count ~ since_intervention + subdistrict + Mode.Transport +
                      (1 | polygon) +
                      offset(log(Track.Length)), 
                    family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

#Drop 1 test
drop1(m1.1_since, test = "Chisq") #drop mode of transport

#Updated model 1 dropping mode.transport
m1_since_final <- glmer(Sighting.Count ~ since_intervention + subdistrict +
                      (1 | polygon) +
                      offset(log(Track.Length)), 
                    family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))

#Drop 1 test
drop1(m1_since_final, test = "Chisq") #all significant

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1_since_final) #1.347, is this close to concerning(?)

#Check with DHARMa
simulationOutput_m1_since <- simulateResiduals(fittedModel = m1_since_final) #create simulated data

testDispersion(simulationOutput_m1_since)

testOutliers(simulationOutput_m1_since)

testZeroInflation(simulationOutput_m1_since)

testUniformity(simulationOutput_m1_since)

plot(simulationOutput_m1_since)

################################################################################

##FIT MODEL TOTAL EFFORT##

#most complex model
m1_total <- glmer(Sighting.Count ~ sc_total + subdistrict + day + Mode.Transport +
                    (1 | polygon) +
                    offset(log(Track.Length)), 
                  family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1_total)

#check VIF
vif(m1_total) #all good

#drop1
drop1(m1_total, test = "Chisq") #all significant

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1_total) #1.939 close to concerning(?)

#Check with DHARMa
simulationOutput_m1_total <- simulateResiduals(fittedModel = m1_total) #create simulated data

testDispersion(simulationOutput_m1_total)

testOutliers(simulationOutput_m1_total)

testZeroInflation(simulationOutput_m1_total)

testUniformity(simulationOutput_m1_total)

plot(simulationOutput_m1_total)

################################################################################

##FIT MODEL EFFORT BY YEAR##

#Check for correlation between year variables
years_effort <- dog_density %>% 
  select(sc_4y, sc_3y,
         sc_2y, sc_1y)

corrplot(cor(years_effort, use = "pairwise.complete.obs"),
         method = "color",
         type = "upper",
         tl.col = "black") #No severe correlation between years

#most complex models
m1_year <- glmer(Sighting.Count ~ sc_4y + sc_3y + sc_2y + sc_1y + subdistrict + day + Mode.Transport +
                    (1 | polygon) +
                    offset(log(Track.Length)), 
                  family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1_year)

#check VIF
vif(m1_year) #all good

#drop 1
drop1(m1_year, test = "Chisq") #drop 1y

#updated model without 1y effort
m1.1_year <- glmer(Sighting.Count ~ sc_4y + sc_3y + sc_2y + subdistrict + day + Mode.Transport +
                   (1 | polygon) +
                   offset(log(Track.Length)), 
                 family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1.1_year)

#drop 1
drop1(m1.1_year, test = "Chisq")#drop 2y

#updated model without 2y effort
m1.2_year <- glmer(Sighting.Count ~ sc_4y + sc_3y + subdistrict + day + Mode.Transport +
                     (1 | polygon) +
                     offset(log(Track.Length)), 
                   family = poisson, data = dog_density,control=glmerControl(optimizer="bobyqa"))
summary(m1.2_year)

#drop 1
drop1(m1.2_year, test = "Chisq")#all significant

################################################################################

##CHECK OVERDISPERSION##

#Check residual deviance relative to degrees of freedom
overdisp.glmer(m1.2_year) #1.882, not great but not terrible (?)

#Check with DHARMa
simulationOutput_m1_year <- simulateResiduals(fittedModel = m1_year) #create simulated data

testDispersion(simulationOutput_m1_year)

testOutliers(simulationOutput_m1_year)

testZeroInflation(simulationOutput_m1_year)

testUniformity(simulationOutput_m1_year)

plot(simulationOutput_m1_year)

################################################################################

##COMPARE MODELS##

#Compare AIC values
AIC(m1_since, m1_total, m1.2_year) #all extremely close

################################################################################

##PLOT MODELS - Since Intervention##

#get predicted values for since intervention model
preds_since <- ggpredict(m1_since, terms = c("since_intervention", "subdistrict"), condition = c(Track.Length = 1))

#plot since intervention model 
ggplot(preds_since, aes(x = x, y = predicted, color = group)) +
           geom_line(linewidth = 1.5) +
           geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_point(
             data = dog_density,
             aes(x = since_intervention, y = Sighting.Count, color = subdistrict),
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

#Predicted values for day
preds_days <- ggpredict(m1_since, terms = c("since_intervention", "day"), condition = c(Track.Length = 1))

#plot day
ggplot(preds_days, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.5) +
  geom_point(
    data = dog_density,
    aes(x = since_intervention, y = Sighting.Count, color = factor(day)),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = "Predicted Sightings/km by\n Time Since Intervention\n and Day",
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

#Predicted values for modes of transport
preds_transport <- ggpredict(m1_since, terms = c("since_intervention", "Mode.Transport"), condition = c(Track.Length = 1))

#plot transport modes
ggplot(preds_transport, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.5) +
  geom_point(
    data = dog_density,
    aes(x = since_intervention, y = Sighting.Count, color = Mode.Transport),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = "Predicted Sightings/km by\n Time Since Intervention\n and Mode of Transport",
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

##PLOT MODELS - Total Effort##

#get predicted values for total effort
preds_total <- ggpredict(m1_total, terms = c("sc_total", "subdistrict"), condition = c(Track.Length = 1))

#plot total effort
ggplot(preds_total, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.5) +
  geom_point(
    data = dog_density,
    aes(x = sc_total, y = Sighting.Count, color = subdistrict),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = "Predicted Sightings/km by\n Total Effort (Scaled)\n and Subdistrict",
       x = "Total Dog Sterilizations (per Human Capita)",
       y = "Predicted Sightings per Km") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 22))

#Predicted values for day
preds_total_days <- ggpredict(m1_total, terms = c("sc_total", "day"), condition = c(Track.Length = 1))

#plot day
ggplot(preds_total_days, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.5) +
  geom_point(
    data = dog_density,
    aes(x = sc_total, y = Sighting.Count, color = factor(day)),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = "Predicted Sightings/km by\n Total Sterilization Effort (scaled)\n and Day",
       x = "Total Dog Sterilizations (per Human Capita)",
       y = "Predicted Sightings per Km") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 22))

#Predicted values for modes of transport
preds_total_transport <- ggpredict(m1_total, terms = c("sc_total", "Mode.Transport"), condition = c(Track.Length = 1))

#plot transport modes
ggplot(preds_total_transport, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.5) +
  geom_point(
    data = dog_density,
    aes(x = sc_total, y = Sighting.Count, color = Mode.Transport),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = "Predicted Sightings/km by\n Total Effort (scaled)\n and Mode of Transport",
       x = "Total Dog Sterilizations (per Human Capita)",
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

##PLOT MODELS - Effort by YEAR## 

#get predicted values for effort by year
preds_year4 <- ggpredict(m1.2_year, terms = c("sc_4y", "subdistrict"), condition = c(Track.Length = 1))

#plot effort 4y ago
ggplot(preds_year4, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.5) +
  geom_point(
    data = dog_density,
    aes(x = sc_4y, y = Sighting.Count, color = subdistrict),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = "Predicted Sightings/km by\n Sterilization Effort 4 Years Ago (scaled)\n and Subdistrict",
       x = "Total Sterilizations 4 Years Ago (Per Human Capita)",
       y = "Predicted Sightings per Km") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 22))

#get predicted values for effort by year
preds_year3 <- ggpredict(m1.2_year, terms = c("sc_3y", "subdistrict"), condition = c(Track.Length = 1))

#plot effort 4y ago
ggplot(preds_year3, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.5) +
  geom_point(
    data = dog_density,
    aes(x = sc_3y, y = Sighting.Count, color = subdistrict),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = "Predicted Sightings/km by\n Sterilization Effort 3 Years Ago (scaled)\n and Subdistrict",
       x = "Total Sterilizations 3 Years Ago (Per Human Capita)",
       y = "Predicted Sightings per Km") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 22))
