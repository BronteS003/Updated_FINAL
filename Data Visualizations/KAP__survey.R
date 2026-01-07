################################################################################
##                     Summary of KAP Survey Results                          ##
################################################################################
# Count of total survey, surveys from each district, and survey response rates #
################################################################################
## Created Dec. 17, 2025, by Bronte Slote, last edited Jan. 6, 2026           ##
################################################################################

##Load Libraries##
library(readr)
library(dplyr)

################################################################################

##Load Data##
KAP <- read.csv("C:/Users/bront/OneDrive/Final_Project/Updated_FINAL/Data/KAP.csv")

################################################################################

##Organize Data##
clean <- KAP %>%
  filter(
    `Is.someone..18.present.at.the.household.` != "No",
    `Has.the.respondent.consented.to.undertake.the.survey.` != "No")


################################################################################

##Survey Counts##

KAP %>% count(District)
clean %>% count(Religion)
clean %>% count(Gender)
clean %>% count(Do.you.currently.own.any.dogs...not.including.street.dogs.you.provide.care.for.)
clean %>% count(In.the.last.week..have.you.someone.in.your.household.offered.care.to.a.street.dog.)

################################################################################

##Find Non-Response Rate##

#number of incomplete surveys/total number of surveys*100
14/330*100 # = 4.24

#number of incomplete surveys from each district
KAP %>%
  group_by(
    District,
    adult_present = `Is.someone..18.present.at.the.household.` != "No",
    consented = `Has.the.respondent.consented.to.undertake.the.survey.` != "No"
  ) %>%
  summarise(n = n(), .groups = "drop")

