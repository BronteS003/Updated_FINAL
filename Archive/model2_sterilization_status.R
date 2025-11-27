################################################################################
#             Model 2: Change in Sterilization Status Overtime                 #
################################################################################
# Data from sight and resight surveys as recorded in WVS                       #
################################################################################
# Created June 7, 2025 by Bronte Slote, last modified Aug. 21, 2025             #
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

sightings <- readRDS("sightings.rds", refhook = NULL)

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
m2_since_final<- glmer(Neutered ~ since_intervention + owned + sex +
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

#Final model for total effort
m2_total_final <- glmer(Neutered ~ effort_humanpop + owned + sex +
                          (1 | polygon),
                        family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))


#Effort by year

#Create first model using effort by year
m2_year <- glmer(Neutered ~ effort_3y_humanpop + effort_2y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m2_year)#drop effort 2y
m2_year <- glmer(Neutered ~ effort_3y_humanpop + effort_1y_humanpop + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
vif(m2_year)#all fine now
drop1(m2_year, test = "Chisq")

#Drop subdistrict
m2_1year <- glmer(Neutered ~ effort_3y_humanpop + effort_1y_humanpop + owned + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_1year, test = "Chisq")

#Final model for effort by year
m2_year_final <- glmer(Neutered ~ effort_3y_humanpop + effort_1y_humanpop + owned + sex +
                         (1 | polygon),
                       family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))


#Compare three sterilization effort indicators
AIC(m2_since_final, m2_total_final, m2_year_final) #total effort has the lowest AIC


##CHECK FOR OVERDISPERSION##

#Visually check overdispersion using DHARMa plot
simulationOutput_model2 <- simulateResiduals(fittedModel = m2_total_final) #create simulated data
testDispersion(simulationOutput_model2)

#Test for outliers
testOutliers(simulationOutput_model2)

#Check for zero inflation
testZeroInflation(simulationOutput_model2)



##PLOT MODELS##

# Get predicted values over "owned"
preds_owned <- ggpredict(m2_total_final, terms = c("owned"))

# Plot Probability of Being Neutered by Sterilizations ownership status 
g1 <- ggplot(preds_owned, aes(x = x,
                        y = predicted)) +
  geom_col(width = 0.6, fill = "steelblue") +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                width = 0.2) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(
    title = "Predicted Probability of Being Neutered\nby Ownership Status",
    x     = "Owned",
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


# Get predicted values over "sex"
preds_sex <- ggpredict(m2_total_final, terms = c("sex"))

# Plot Probability of Being Neutered by sex

library(scales)

g2 <- ggplot(preds_sex, aes(x = x, y = predicted)) +
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

#create final graph by total sterilization effort
# Get predicted values over "effort_humanpop"
preds_effort <- ggpredict(m2_total_final, terms = c("effort_humanpop"))

# Plot Probability of Being Neutered by Sterilization effort 
ggplot(preds_effort, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey70", alpha = 0.5) +
  geom_line(size = 1.2, color = "steelblue") +
  labs(title = "Probability of Being Neutered by\n Total Sterilization Effort",
       x = "Total Sterilization Effort (Per Capita)",
       y = "Probability of Being Neutered") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12,face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  scale_fill_viridis_d(option = "C", end = 0.9) +
  scale_y_continuous(breaks = c(0,0.2, 0.4, 0.6, 0.8), limits = c(0, 1))


g3 <- plot(preds_effort) + 
  labs(title = "Predicted Probability of Being Neutered \n by Sterilization Effort",
       x = "Total Sterilization Effort (Per Capita)",
       y = "Predicted Probability") +
  theme_minimal(base_size = 14)  +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank())

#Combine into one panel 
g1 + g2 + g3 +
  plot_annotation(tag_levels = 'A')



#Plot total effort by time 

#Create predicted values 
preds_year <- ggpredict(m2_since_final, terms = c("since_intervention"))

ggplot(preds_year, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey70", alpha = 0.5) +
  geom_line(size = 1.2, color = "steelblue") +
  labs(title = "Probability of Being Neutered by Year",
       x = "Time Since Intervention (Year)",
       y = "Probability of Being Neutered") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  scale_fill_viridis_d(option = "C", end = 0.9)+ 
  scale_y_continuous(breaks = c(0,0.2, 0.4, 0.6, 0.8), limits = c(0, 1))

#Plots with plot function

plot(preds_year) + 
  labs(title = "Predicted Probability of Being Neutered",
       x = "Time Since Intervention (Years)",
       y = "Predicted Probability") +
  theme_minimal(base_size = 14)  +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank())
