################################################################################
#             FULL Model 2: Change in Sterilization Status Overtime            #
################################################################################
# Data from sight and resight surveys, updated data (2021-2025)                #
################################################################################
# Created Oct 15, 2025 by Bronte Slote, last modified Oct.15, 2025             #
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
library(emmeans)
library(patchwork)

##IMPORT DATA##

#Read rds for sightings file

sightings <- readRDS("FULL_sightings.rds", refhook = NULL)

##CLEAN DATA##

#Remove puppies as they're not relevant to analysis
sightings <- sightings %>% 
  filter(Puppy != 1)

#Remove adults with unknown sterilization status 
sightings <- sightings %>% 
  filter(Unknown != 1)

##MODEL SELECTION##

#Time Since Intervention

#Create first model using time since intervention
m2_since <- glmer(Neutered ~ since_intervention + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
#Check vif
vif(m2_since) #all fine

#Test which variable to drop
drop1(m2_since, test = "Chisq")

#Drop subdistrict
m2_1since <- glmer(Neutered ~ since_intervention + owned + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
#Test which variable to drop
drop1(m2_1since, test = "Chisq")#all variable significant

#Final model for time since intervention
m2_final_since <- glmer(Neutered ~ since_intervention + owned + sex +
                         (1 | polygon),
                       family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#Total Effort

#Create first model using total effort
m2_total <- glmer(Neutered ~ effort_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
#Check vif
vif(m2_total)# all fine

#Check which variables to drop
drop1(m2_total, test = "Chisq")

#Drop subdistrict
m2_1total <- glmer(Neutered ~ effort_humanpop + owned + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
#Check which variables to drop
drop1(m2_1total, test = "Chisq")# all significant

#Drop owned
m2_2total <- glmer(Neutered ~ effort_humanpop + sex +
                     (1 | polygon),
                   family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
#Check which variables to drop
drop1(m2_2total, test = "Chisq")# all significant

#Final model for total effort
m2_final_total <- glmer(Neutered ~ effort_humanpop + sex +
                          (1 | polygon),
                        family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))


#Effort by year

#Create first model using effort by year
m2_year <- glmer(Neutered ~ effort_4y_humanpop + effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m2_year)#all good
#Check what variables to drop
drop1(m2_year, test = "Chisq")

#Drop 4y
m2_1year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_1year, test = "Chisq")

#Drop subdistrict
m2_2year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_2year, test = "Chisq")

#Drop owned
m2_3year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_3year, test = "Chisq")

#Final model for effort by year
m2_final_year <-  glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + sex +
                         (1 | polygon),
                       family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))


#Compare three sterilization effort indicators
AIC(m2_final_since, m2_final_total, m2_final_year) #year has the lowest AIC


##CHECK FOR OVERDISPERSION##

#Visually check overdispersion using DHARMa plot
simulationOutput_model2 <- simulateResiduals(fittedModel = m2_year_final) #create simulated data
testDispersion(simulationOutput_model2)

#Test for outliers
testOutliers(simulationOutput_model2)

#Check for zero inflation
testZeroInflation(simulationOutput_model2)

#Check for uniformity
testUniformity(simulationOutput_model2)

##PLOT MODELS##

# Get predicted values over "sex"
preds_sex <- ggpredict(m2_final_year, terms = c("sex"))

# Plot Probability of Being Neutered by sex

library(scales)

ggplot(preds_sex, aes(x = x, y = predicted)) +
  geom_col(width = 0.6, fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2) +
  scale_x_discrete(labels = c("F", "M")) +
  labs(
    title = "Predicted Probability of Being Neutered\nby Sex",
    x     = "Sex",
    y     = "Predicted Probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),   
    breaks = seq(0, 1, 0.2),                      
    limits = c(0, 0.9)
  )


#create plot for probability of being sterilized by effort

#Get predicted values over effort
preds_effort <- ggpredict(m2_final_total, terms = "effort_humanpop")

#Plot
plot(preds_effort) +
  labs(
    title = "Predicted Probability of Being Neutered \n by Sterilization Effort",
    x = "All Time Effort (Per Capita)",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)  # ← bold + centered
  )


#Create plot for probability of being sterilized by time since intervention

# Get predicted values over time since
preds_since <- ggpredict(m2_final_since, terms = c("since_intervention"))

# Plot Probability of Being Neutered by years since intervention
plot(preds_since) + 
  labs(
    title = "Predicted Probability of Being Neutered \n by Time Since Intervention",
    x = "Time Since Intervention",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)  # ← bold + centered
  )




##Plot Probability of Being Neutered by effort by year

#Create predicted values for 3 years ago
preds_3y <- ggpredict(m2_final_year, terms = "effort_3y_humanpop", type = "fixed")

plot(preds_3y) +
  labs(
    title = "Predicted Probability of Being Neutered\nvs Sterilization Effort 3 Years Ago",
    x = "Sterilizations per Human Capita (3 Years Ago)",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Create predicted values for 2 years ago
preds_2y <- ggpredict(m2_final_year, terms = "effort_2y_humanpop", type = "fixed")

plot(preds_2y) +
  labs(
    title = "Predicted Probability of Being Neutered\nvs Sterilization Effort 2 Years Ago",
    x = "Sterilizations per Human Capita (2 Years Ago)",
    y = "Probability of Being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Create predicted values for 1 years ago
preds_1y <- ggpredict(m2_final_year, terms = "effort_1y_humanpop", type = "fixed")

plot(preds_1y) +
  labs(
    title = "Predicted Probability of Being Neutered\nvs Sterilization Effort 1 Year Ago",
    x = "Sterilizations per Human Capita (1 Year Ago)",
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
    title = "Probability of Being Neutered by Past Sterilization Effort",
    x = "Sterilizations per Human Capita",
    y = "Probability of being Neutered"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_color_manual(values = c("3 Years Ago" = "#2E86AB", "2 Years Ago" = "#E07A5F", "1 Year Ago" = "darkgreen")) +
  scale_fill_manual(values = c("3 Years Ago" = "#2E86AB", "2 Years Ago" = "#E07A5F", "1 Year Ago" = "darkgreen"))
