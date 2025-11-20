################################################################################
##                      Comparison Between Days                               ##
################################################################################
## Comparisons between day 1 and day 2 of dog sightings                       ##
################################################################################
## Created Nov. 18, 2025, by Bronte Slote, last edited Nov. 18, 2025          ##
################################################################################


##LOAD LIBRARIES##
library(readr) #reading csv files
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(stringr) #manipulating text
library(tidyr)
library(ggeffects)
library(glmmTMB)
library(DHARMa)

##IMPORT DATA##

##IMPORT AND CLEAN DATA##

##Import Data set "sightings"
sightings <- read.csv("FULL_sightings.csv")

##Rename columns
sightings <- sightings %>% rename("polygon" = "Sandbox.Name",#rename so easier to remember
                                  "date"= "Timestamp")

#Recognize polygon as a categorical variable with 7 levels
sightings <- sightings %>%
  mutate(polygon = factor (polygon, levels = c("Khok Kruat 01","Khok Kruat 06","Khok Kruat 07", "Tha Chang 12", "Tha Chang 16","Tha Chang 20","Tha Chang 24")))

#Rename long form of polygon name to short form
sightings <- sightings %>%
  mutate(polygon = str_replace(polygon, "^Khok Kruat", "KK"),
         polygon = str_replace(polygon, "^Tha Chang", "TC"))

#Define column Neutered as numerical
sightings <- sightings %>%
  mutate(Neutered = as.numeric(Neutered)) #make "Neutered" numerical

#Create subdstrict column
sightings$subdistrict <- str_extract(sightings$polygon, "^[A-Za-z]+") #extract the letters from the "polygon" column and make them a new column "subdistrict"

#Define subdistrict as categorical with 2 levels
sightings <- sightings %>%
  mutate(subdistrict = factor(subdistrict, levels = c("KK","TC"))) #make "subdistrict" a factor with the levels 'KK' and 'TC'

#Create survey identifier based on polygon and year of survey
sightings <- sightings %>%
  mutate(
    year = format(as.Date(date), "%Y"),  # create a new column isolating year
    survey = paste(polygon, year, sep = "_")  # create new column combining polygon and year to create a unique identifier for surveys
  )

#Make survey a factor variable  
sightings <- sightings %>%
  mutate(survey = as.factor(survey))

#Make column for resight
sightings <- sightings %>%
  mutate(
    notes_clean = str_to_lower(Notes),
    resight = case_when(
      notes_clean == "resight"   ~ "Yes",
      notes_clean == "new_dog"   ~ "No",
      notes_clean %in% c("", NA, "blank") ~ "No",  
      notes_clean == "unknown"   ~ "unknown",               
      TRUE                       ~ NA                
    )
  ) %>%
  select(-notes_clean)

#Convert date to a date variable
sightings <- sightings %>%
  mutate(date = substr(date, 1, 10))#just take date

#Recognize date as date
sightings <- sightings %>%
  mutate(date = as.Date(date))

#Find duplicate dates
dup_rows <- sightings %>%
  group_by(survey, date) %>%
  summarise(n = n(), .groups = "drop")

#Correct dates for TC 12_2024 
sightings <- sightings %>%
  mutate(date = if_else(
    survey == "TC 12_2024" & Notes %in% c("new_dog", "resight"),
    date + days(2),
    date
  ))

#Create "since_intervention" column
sightings <- sightings %>%
  mutate(
    intervention_start = case_when( #create column "intervention_start"
      subdistrict == "KK" ~ as.Date("2022-02-11"), #where the subdistrict is "KK" make the intervention start date as 2022-02-11
      subdistrict == "TC" ~ as.Date("2022-11-17") #where the subdistrict is "TC" make the intervention start date as 2022-11-17
    ),
    since_intervention = as.numeric(date - intervention_start) #create a new numeric column "since_intervention" by subtracting intervention start date from date of survey resulting ina column showing number of days since intervention
  )

#Make days since intervention to years
sightings <- sightings %>%
  mutate(since_intervention = since_intervention / 365)


#Create variable "day" to show either day 1 or 2 survey
sightings <- sightings %>%
  mutate(day = case_when( #create new column "day"
    polygon == "KK 01" & date %in% as.Date(c("2021-10-07", "2023-03-01", "2024-08-27", "2025-09-04")) ~ 1,
    polygon == "KK 06" & date %in% as.Date(c("2021-10-07", "2023-02-28", "2024-08-27", "2025-09-04")) ~ 1,
    polygon == "KK 07" & date %in% as.Date(c("2021-10-07", "2023-02-28", "2024-08-27", "2025-09-04")) ~ 1,
    polygon == "TC 12" & date %in% as.Date(c("2023-03-17", "2024-09-10", "2025-09-18")) ~ 1,
    polygon == "TC 16" & date %in% as.Date(c("2023-03-17", "2024-09-10", "2025-09-10")) ~ 1,
    polygon == "TC 20" & date %in% as.Date(c("2023-03-17", "2024-09-10", "2025-09-18")) ~ 1,
    polygon == "TC 24" & date %in% as.Date(c("2023-03-17", "2024-09-10", "2025-09-18")) ~ 1,
    TRUE ~ 2  # everything else gets "2"
  ))

#Make day a factor variable  
sightings <- sightings %>%
  mutate(day= as.factor(day))

##Comparison of Day##

##compute_counts function
compute_counts <- function(df, treat_unknown_as_no = FALSE) {
#create resight variable
  df <- df %>%
    mutate(resight2 = case_when(
      treat_unknown_as_no & resight == "unknown" ~ "no",
      TRUE ~ resight
    ))
# calculate counts
  tibble(
    n1 = sum(df$day == 1, na.rm = TRUE),
    n2 = sum(df$day == 2, na.rm = TRUE),
    m2 = sum(df$day == 2 & df$resight2 == "Yes", na.rm = TRUE),
    unknown_day2 = sum(df$day == 2 & df$resight == "unknown", na.rm = TRUE)
  )
}


##Chapman estimator function
chapman_estimator <- function(n1, n2, m2) {
  # Chapman estimate of population size
  Nhat <- ((n1 + 1) * (n2 + 1) / (m2 + 1)) - 1
  
  # Standard error
  SE <- sqrt((n1 + 1) * (n2 + 1) * (n1 - m2) * (n2 - m2) /
               ((m2 + 1)^2 * (m2 + 2)))
  
  # 95% confidence interval
  CI <- c(Nhat - 1.96 * SE, Nhat + 1.96 * SE)
  
  list(
    Nhat = Nhat,
    SE = SE,
    CI_low = CI[1],
    CI_high = CI[2]
  )
}


#Full results
results_df <- sightings %>%
  group_by(survey) %>%
  summarise(
    counts = list(compute_counts(cur_data(), treat_unknown_as_no = FALSE)),
    .groups = "drop"
  ) %>%
  unnest_wider(counts) %>%   # expands n1, n2, m2, unknown_day2 into columns
  rowwise() %>%              # safe for calling estimator
  mutate(
    result = list(
      # Conditions
      if (n1 == 0 | n2 == 0) {
        tibble(Nhat = NA, SE = NA, CI_low = NA, CI_high = NA,
               Note = "needs >=1 obs on both days")
      } else if (m2 == 0) {
        tibble(Nhat = NA, SE = NA, CI_low = NA, CI_high = NA,
               Note = "m2 == 0 (no recaptures)")
      } else {
        # Compute Chapman estimate
        est <- chapman_estimator(n1, n2, m2)
        tibble(
          Nhat = est$Nhat,
          SE = est$SE,
          CI_low = est$CI_low,
          CI_high = est$CI_high,
          Note = NA
        )
      }
    )
  ) %>%
  unnest(result) %>%
  ungroup()

print(results_df)



##Day Comparison - paired count with t-testand modelling##

# create per-survey counts (one row per survey)
counts_by_survey <- sightings %>%
  group_by(survey, day) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = day, values_from = n, values_fill = 0, names_prefix = "day") %>%
  rename(n1 = day1, n2 = day2)

#Set paired argument 
t.test(counts_by_survey$n1, counts_by_survey$n2, paired = TRUE)


#Model
# Make long counts table: one row per survey-day (so two rows per survey)
survey_day <- sightings %>%
  mutate(day = factor(day, levels = c(1, 2))) %>%  # make sure day is factor
  count(survey, day) %>%
  tidyr::complete(survey, day = factor(c(1, 2), levels = c(1, 2)), 
                  fill = list(n = 0)) %>%
  rename(count = n) %>%
  mutate(dayf = day)

# Fit poisson model
m_pois_fe <- glmmTMB(count ~ dayf + (1 | survey), family = poisson, data = survey_day)
summary(m_pois_fe)

#Check model fit

#Test dispersion
testDispersion(m_pois_fe)

#Test residuals
simulationOutput <- simulateResiduals(fittedModel = m_pois_fe, plot = F)
plotQQunif(simulationOutput)
plotResiduals(simulationOutput)

#Plot 
preds <- ggpredict(m_pois_fe, terms = c("dayf", "survey"),
                    condition = c(Track.Length=1))
ggplot(preds, aes(x = x, y = predicted)) + 
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "grey70", color = NA) +
  facet_wrap(~ group) +
  labs(title = "Predicted Sightings by Day",
       x = "Day",
       y = "Predicted Sightings per Km") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )


