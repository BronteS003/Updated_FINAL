################################################################################
#             Model 2: Change in Sterilization Status Overtime                 #
################################################################################
# Data from sight and resight surveys as recorded in WVS                       #
################################################################################
# Created June 7, 2025 by Bronte Slote, last modified July 17, 2025             #
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

##MODEL SELECTION##

#Time Since Intervention

#Create first model using time since intervention
m2_since <- glmer(Neutered ~ since_intervention + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_since, test = "Chisq")

#Final model for time since intervention
m2_since_final<- glmer(Neutered ~ since_intervention + owned + subdistrict + sex +
                         (1 | polygon),
                       family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))

#Total Effort

#Create first model using total effort
m2_total <- glmer(Neutered ~ effort_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_total, test = "Chisq")

#Final model for total effort
m2_total_final <- glmer(Neutered ~ effort_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))


#Effort by year

#Create first model using effort by year
m2_year <- glmer(Neutered ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + owned + subdistrict + sex +
                    (1 | polygon),
                  family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))
drop1(m2_year, test = "Chisq")

#Final model for effort by year
m2_year_final <- glmer(Neutered ~ last3y_humanpop + last2y_humanpop + last1y_humanpop + owned + subdistrict + sex +
                   (1 | polygon),
                 family = binomial, data = sightings, control = glmerControl(optimizer = "bobyqa"))


#Compare three sterilization effort indicators
AIC(m2_since_final, m2_total_final, m2_year_final) #year model has the lowest AIC

#Conduct posthoc test
emmeans(m2_year_final,pairwise ~ sex) #none of the interactions are significant, does this means we should drop sex?

##CHECK FOR OVERDISPERSION##

#Visually check overdispersion using DHARMa plot
simulationOutput_model2 <- simulateResiduals(fittedModel = m2_year_final) #create simulated data
testDispersion(simulationOutput_model2)

#Test for outliers
testOutliers(simulationOutput_model2)

#Check for zero inflation
testZeroInflation(simulationOutput_model2)



##PLOT MODELS##

# Get predicted values over "owned" and "subdistrict"
preds_owned <- ggpredict(m2_year_final, terms = c("owned", "subdistrict"))

# Plot Probability of Being Neutered by Sterilizations ownership status and subdistrict
g1 <- ggplot(preds_owned, aes(x = x,
                      y = predicted,
                      fill = group)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.6) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                position = position_dodge(width = 0.5),
                width = 0.2) +
  scale_x_discrete(labels = c("Yes", "No")) +
  labs(
    title = "Predicted Probability of Being Neutered\nby Ownership Status and Subdistrict",
    x     = "Owned",
    y     = "Predicted Probability",
    fill  = "Subdistrict"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  scale_fill_viridis_d(option = "C", end = 0.9)



# Get predicted values over "sex" and "subdistrict"
preds_sex <- ggpredict(m2_year_final, terms = c("sex", "subdistrict"))

# Plot Probability of Being Neutered by sex and subdistrict
g2 <- ggplot(subset(preds_sex, group != "unknown"), aes(x = x,
                                                  y = predicted,
                                                  fill = group)) +
  geom_col(position = position_dodge(width = 0.5), width = 0.6) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                position = position_dodge(width = 0.5),
                width = 0.2) +
  scale_x_discrete(labels = c("Female", "Male")) +
  labs(
    title = "Predicted Probability of Being Neutered\nby Sex and Subdistrict",
    x     = "Sex",
    y     = "Predicted Probability",
    fill  = "Subdistrict"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  scale_fill_viridis_d(option = "C", end = 0.9)

#create final graph for sterilization by years

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
m2_final_long <- glmer(
  Neutered ~ sterilization_effort * YearBreakdown + subdistrict + (1 | polygon),
  family = binomial,
  data = sightings_long,
  control = glmerControl(optimizer = "bobyqa")
)

#Predicted values for m2_final_long over "YearBreakdown" and "subdistrict"
preds2 <- ggpredict(m2_final_long, terms = c("YearBreakdown", "subdistrict"))

# Plot
g3 <- ggplot(preds2, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1) +  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  scale_x_reverse(breaks = c(3, 2, 1)) +
  labs(
    title = "Predicted Probability of Being Neutered\nby Year",
    x = "Years Ago",
    y = "Probability of Being Neutered",
    color = "Subdistrict",
    fill = "Subdistrict"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  scale_fill_viridis_d(option = "C", end = 0.9)

#Combine into one panel 
g1 + g2 + g3

#Identify exact data points from the plot
plot_data <- ggplot_build(g3)$data
plot_data


