################################################################################
##              Proportion of Male & Female in Sample                         ##
################################################################################
## Male:Female ratio in sight/resight survey data, to understand if there is  ##
## a disproportionate number of one sex in the population and if this is      ##
## reflected in the number of dogs sterilized in clinics                      ##
################################################################################

##LOAD LIBRARIES##
library(dplyr)

################################################################################

##LOAD DATA##

#Sightings
sightings <- readRDS("Models/FULL_sightings.rds", refhook = NULL)

#Remove unknown sex
clean_sightings <- sightings %>% 
  filter(sex != "Unknown")

#Clinic Data

clinic <- readRDS ("Models/FULL_clinic_data.rds", refhook = NULL)

#Reformat sex column to M or F
clean_clinic <- clinic %>%
  mutate(sex = toupper(substr(sex, 1, 1)))

################################################################################

## M:F Ratio - Sightings ##

#Count by sex and calculate percentage
ratio_data <- clean_sightings %>%
  group_by(sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

#Calculate M:F ratio
male_count <- ratio_data %>% filter(sex == "M") %>% pull(count)
female_count <- ratio_data %>% filter(sex == "F") %>% pull(count)

male_female_ratio <- (male_count / female_count) * 100
cat("Male to Female Ratio (per 100 females):", male_female_ratio, "\n")

#Test if significant difference between proportion of males to females
prop.test(c(240, 279), c(519, 519))
#Differ significantly

################################################################################

## M:F Ratio - Clinic Data ##

#Count by sex and calculate percentage
clc_ratio_data <- clean_clinic %>%
  group_by(sex) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

#Calculate M:F ratio
clc_male_count <- clc_ratio_data %>% 
  filter(sex %in% c("M")) %>% 
  pull(count)

clc_female_count <- clc_ratio_data %>% 
  filter(sex %in% c("F")) %>% 
  pull(count)

clc_male_female_ratio <- (clc_male_count / clc_female_count) * 100
cat("Male to Female Ratio (per 100 females):", clc_male_female_ratio, "\n")

#Test if significant difference between proportion of males to females
prop.test(c(794, 892), c(1686, 1686))
#statistically different

################################################################################


