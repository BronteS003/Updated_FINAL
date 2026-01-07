################################################################################
##                Health Conditions By Group                                  ##
################################################################################
## Checking health conditions by group in sightings data.                     ##
################################################################################
## Created by Bronte Slote, Dec. 12, 2025, last updated Dec. 12, 2025         ##
################################################################################

##LOAD LIBRARIES##
library(dplyr)
library(ggplot2)
library(lme4)

################################################################################

##IMPORT & ORGANIZE DATA - SIGHTINGS##

#Read RDS file##
sum_density <- readRDS("Data/FULL_dog_density.rds", refhook = NULL)

#Remove all healthy and unknown health status dogs from count
sick_density <- sum_density %>%
  mutate(
    Sick.Count = Sighting.Count - Healthy - Unknown.health
  )

#Create owned as proportion
sick_density$prop_owned <- sick_density$Owned / 
  (sick_density$Owned + sick_density$Free.roaming.NO.collar)

#Create Female column
sick_density <- sick_density %>%
  mutate(Female = rowSums(across(c(Adult.NON.lactating.female, Adult.Lactating.female)), na.rm = TRUE))

#Create sex as a proportion
sick_density$prop_female <- sick_density$Female / 
  (sick_density$Female + sick_density$Adult.male)

#Create column for condition categories - BCS, Infectious, Injury

##PRELIMINARY VISUALISATION##

#Count by year & subdistrict
ggplot(sick_density, aes(x = year, y = Sick.Count, group = subdistrict)) +
  geom_point()

##MODEL - Sick Count##

m_owned <- glmer(Sick.Count ~ prop_owned + (1 | polygon),
                 offset = log(Sighting.Count),
            family = poisson, data = sick_density, control = glmerControl(optimizer = "bobyqa"))
summary(m_owned)

m_sex <- glmer(Sick.Count ~ prop_female + (1 | polygon),
               offset = log(Sighting.Count),
               family = poisson, data = sick_density, control = glmerControl(optimizer = "bobyqa"))
summary(m_sex)

################################################################################

##COUNTS##

sick_sightings %>% count(sex)

################################################################################

##IMPORT & ORGANIZE DATA - SIGHTINGS##

#Read RDS file##
clinic <- readRDS("Data/FULL_clinic_data.rds", refhook = NULL)

################################################################################

##Create data set from dogs that have one or more condition##

#Identify target conditions
target_conditions <- c("underweight", "obese", "emaciated", "overweight", "severe_skin_dz", "mild_skin_dz",
                       "mod_skin_dz","pyo", "tvt", "wounds_maggots", "yes_compl")
sick_clinic <- clinic %>%
  filter(if_any(everything(), ~ .x %in% target_conditions))

##How many conditions each dog has
condition_count <- sick_clinic %>%
  mutate(
    n_conditions = rowSums(
      as.data.frame(across(everything(), ~ .x %in% target_conditions))
    )
  )
condition_count %>% count(n_conditions)

################################################################################

##PLOT BY YEAR##

ggplot(sick_clinic, aes(x = Year)) +
  geom_bar() +
  facet_wrap(~ subdistrict)
