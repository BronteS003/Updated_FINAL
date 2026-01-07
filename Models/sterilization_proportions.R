################################################################################
##      Sterilization Rate Across Time as Proportion                          ##
################################################################################
## Modeling sterilization rate, based on dog observations as proportions      ##
## of the population                                                          ##
################################################################################
## Created Nov. 28, 2025 by Bronte Slote, last edited Nov. 28, 2025           ##
################################################################################

##Load Libraries##

library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(lme4) #creating mixed models with random effect
library(stringr) #manipulating text

################################################################################

##Import Data##
#Read rds for sightings file

sightings <- readRDS("FULL_sightings.rds", refhook = NULL)

###############################################################################

##Clean Data##

#Remove dogs of unknown sterilization status
clean_sightings <- sightings %>%
  filter(Unknown != 1)

###############################################################################

##Model Selection##

#Model 2.1 - Since Intervention
m2.1 <- glmer(cbind(Neutered, Entire) ~ since_intervention + subdistrict +
  (1|polygon),
family = binomial, data = clean_sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.1)

#Model 2.2 - Total Effort
m2.2 <- glmer(cbind(Neutered, Entire) ~ effort_humanpop +
                (1|polygon),
              family = binomial, data = clean_sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.2)

#Model 2.3 - Effort by Year
m2.3 <- glmer(cbind(Neutered, Entire) ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict +
                (1|polygon),
              family = binomial, data = clean_sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.3)
vif(m2.3)
drop1(m2.3, test = "Chisq") #drop year 4
m2.3.1 <- glmer(cbind(Neutered, Entire) ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + subdistrict +
                (1|polygon),
              family = binomial, data = clean_sightings, control = glmerControl(optimizer = "bobyqa"))
summary(m2.3.1)

################################################################################

##Compare Model Fit##
AIC(m2.1, m2.2, m2.3.1) #year has lowest AIC value

################################################################################

##CHECK FOR OVERDISPERSION##

#Visually check overdispersion using DHARMa plot
simulationOutput_model2 <- simulateResiduals(fittedModel = m2.3.1) #create simulated data
testDispersion(simulationOutput_model2)

#Test for outliers
testOutliers(simulationOutput_model2)

#Check for zero inflation
testZeroInflation(simulationOutput_model2)

#Check for uniformity
testUniformity(simulationOutput_model2)

###############################################################################

##Plotting##

##Plot m2.1 Since Intervention

#Get predicted values for m2.1
pred2.1 <- ggpredict(m2.1, terms = c("since_intervention", "subdistrict"))

#Plot m2.1
plot(pred2.1) + 
  labs(title = "Proportion of Population Sterilized By Time Since Peak of Intervention and Subdistrict",
       x = "Time Since Intervention (Years)",
       y = "Proportion of Population Sterilized") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 1))

#Create plot with raw data points
raw_avg <- clean_sightings %>%
  group_by(survey) %>%
  summarise(
    mean_prop = sum(Neutered) / sum(Neutered + Entire),
    since_intervention = first(since_intervention),
    subdistrict = first(subdistrict),
    .groups = "drop"
  )

#Plot with raw data
plot(pred2.1) +
  geom_point(
    data = raw_avg,
    aes(x = since_intervention, y = mean_prop, color = subdistrict),
    alpha = 0.5,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(title = "Proportion of Dogs Sterilized by\n Time Since Intervention\n and Subdistrict",
       x = "Time Since Intervention (Years)",
       y = "Proportion Sterilized") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 1))

pred2.1 <- ggpredict(
  m2.1,
  terms = c("since_intervention [0, 1, 2, 3]"),
  condition = c(Track.Length = 1)
)
