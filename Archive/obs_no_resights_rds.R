################################################################################
##           All Dog Observations(resights removed)                           ##
################################################################################
# Script for .rds file to be used in all v3 models. Resights are removed,      #
# track lengths are added from dog density .csv, and sterilization effort is   #
# quantified from clinic data. All data is filtered to include only the        #
# subdistrict of focus.                                                        #
################################################################################
# Created April 14, 2026 by Bronte Slote, last edited April 14, 2026           #
################################################################################

##LOAD LIBRARIES##
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

################################################################################

##LOAD DATA##

#raw clinic
raw_clinic <- read.csv("Data/FULL_clinic_data.csv")

#raw dog density
raw_dog_density <- read.csv("Data/FULL_dog_density.csv")

#raw dog observations
raw_sightings <- read.csv("Data/FULL_sightings.csv")

################################################################################

##ORGANIZE TIMESTAMP COLUMNS##

#remove time from timestamp
raw_sightings <- raw_sightings %>%
  mutate(Timestamp = substr(Timestamp, 1, 10)) 

#Recognize date as date
raw_sightings <- raw_sightings %>%
  mutate(Timestamp = as.Date(Timestamp))

################################################################################

##ORGANIZE POLYGON COLUMNS##

#rename polygon column
raw_sightings <- raw_sightings %>%
  rename("polygon" = "Sandbox.Name")

#Recognize polygon as a categorical variable with 7 levels
raw_sightings <- raw_sightings %>%
  mutate(polygon = factor (polygon, levels = c("Khok Kruat 01","Khok Kruat 06","Khok Kruat 07", "Tha Chang 12", "Tha Chang 16","Tha Chang 20","Tha Chang 24")))

#Rename long form of polygon name to short form
raw_sightings <- raw_sightings %>%
  mutate(polygon = str_replace(polygon, "^Khok Kruat", "KK"),
         polygon = str_replace(polygon, "^Tha Chang", "TC"))

#Create subdstrict column
raw_sightings$subdistrict <- str_extract(raw_sightings$polygon, "^[A-Za-z]+") #extract the letters from the "polygon" column and make them a new column "subdistrict"

#Define subdistrict as categorical with 2 levels
raw_sightings <- raw_sightings %>%
  mutate(subdistrict = factor(subdistrict, levels = c("KK","TC"))) #make "subdistrict" a factor with the levels 'KK' and 'TC'


#Create survey identifier based on polygon and year of survey
raw_sightings <- raw_sightings %>%
  mutate(
    year = format(as.Date(Timestamp), "%Y"),  # create a new column isolating year
    survey = paste(polygon, year, sep = "_")  # create new column combining polygon and year to create a unique identifier for surveys
  )

#Make survey a factor variable  
raw_sightings <- raw_sightings %>%
  mutate(survey = as.factor(survey))

#Correct dates for TC 12_2024 (showing up as all observations on same day)
raw_sightings <- raw_sightings %>%
  mutate(Timestamp = if_else(
    survey == "TC 12_2024" & Notes %in% c("new_dog", "resight"), #any that are labelled as "new_dog" or "resight" have to be from a 2nd day of surveying
    Timestamp + days(1), #add 1 day for these dogs that were from second day of surveying
    Timestamp
  ))

################################################################################

##REMOVE RESIGHTS##

#remove resights
no_resights_sightings <- raw_sightings %>%
  filter(!str_to_lower(Notes) %in% c("resight", "unknown"))

################################################################################

##CREATE TIME SINCE INTERVENTION VARIABLE##

#Create "since_intervention" column
no_resights_sightings <- no_resights_sightings %>%
  mutate(
    intervention_start = case_when( #create column "intervention_start"
      subdistrict == "KK" ~ as.Date("2022-02-11"), #where the subdistrict is "KK" make the intervention start date as 2022-02-11
      subdistrict == "TC" ~ as.Date("2022-11-17") #where the subdistrict is "TC" make the intervention start date as 2022-11-17
    ),
    since_intervention = as.numeric(Timestamp - intervention_start) #create a new numeric column "since_intervention" by subtracting intervention start date from date of survey resulting ina column showing number of days since intervention
  )

#Convert time since intervention into years instead of days
no_resights_sightings <- no_resights_sightings %>%
  mutate(since_intervention = since_intervention / 365)

################################################################################

##CREATE VARIABLE DAY 1 AND 2##

#Create variable "day" to show either day 1 or 2 survey
no_resights_sightings <- no_resights_sightings %>%
  group_by(survey) %>%
  mutate(day = dense_rank(Timestamp)) %>%
  ungroup()

################################################################################

##CREATE AGE VARIABLE##

#Make column age
no_resights_sightings <- no_resights_sightings %>%
  mutate(age = case_when(
    Adult.male == 1 ~ "Adult",
    Adult.NON.lactating.female == 1 | Adult.Lactating.female == 1 ~ "Adult",
    Adult.unknown.sex == 1 ~ "Adult",
    Puppy == 1 ~ "Puppy"))

#Make age a factor
no_resights_sightings <-no_resights_sightings %>%
  mutate(age = as.factor(age))

################################################################################

##CREATE SEX VARIABLE##

#Make column sex
no_resights_sightings <- no_resights_sightings %>%
  mutate(sex = case_when(
    Adult.male == 1 ~ "M",
    Adult.NON.lactating.female == 1 | Adult.Lactating.female == 1 ~ "F",
    Adult.unknown.sex == 1|Puppy ==1 ~ "Unknown"))

#Make sex a factor
no_resights_sightings <-no_resights_sightings %>%
  mutate(sex = as.factor(sex))

################################################################################

##CREATE OWNERSHIP VARIABLE##

#Make column owned
no_resights_sightings <- no_resights_sightings %>%
  mutate(owned = case_when(
    Free.roaming.NO.collar == 1 ~ "No",
    Free.roaming.collared | Confined.in.yard | On.chain.or.lead == 1 ~ "Yes"))

#Make owned a factor
no_resights_sightings <- no_resights_sightings %>%
  mutate(owned = as.factor(owned))

################################################################################

##CREATE NEUTERING STATUS VARIABLE##

#Define column Neutered as numerical
no_resights_sightings <- no_resights_sightings %>%
  mutate(Neutered = as.numeric(Neutered)) #make "Neutered" numerical

################################################################################

##GET TRACK LENGTH AND MODE OF TRANSPORT FROM DOG DENSITY DATA AND ADD TO SIGHTINGS##

#Ensure date is treated correctly
dog_density <- raw_dog_density %>%
  mutate(Timestamp = parse_date_time(Timestamp, orders = "d B y, H:M"),
         year = format(Timestamp, "%Y"), #make year a 4 number format
         Timestamp = as.Date(Timestamp)) 

#Create new column "Mode.Transport"
dog_density$Mode.Transport <- str_extract(
  dog_density$Notes,
  "walking|4-wheeler|2-wheeler|bicycle"
)

#Make any NAs in Mode.Transport be shown as unknown
dog_density <- dog_density %>%
  mutate(Mode.Transport = if_else(is.na(Mode.Transport), "Unknown", Mode.Transport))

#Make track length into km instead of meters
dog_density<-dog_density %>%
  mutate(Track.Length = Track.Length/1000)

#create survey variable in dog density dataset
#rename polygon column
dog_density <- dog_density %>%
  rename("polygon" = "Sandbox.Name")

#Recognize polygon as a categorical variable with 7 levels
dog_density <- dog_density %>%
  mutate(polygon = factor (polygon, levels = c("Khok Kruat 01","Khok Kruat 06","Khok Kruat 07", "Tha Chang 12", "Tha Chang 16","Tha Chang 20","Tha Chang 24")))

#Rename long form of polygon name to short form
dog_density <- dog_density %>%
  mutate(polygon = str_replace(polygon, "^Khok Kruat", "KK"),
         polygon = str_replace(polygon, "^Tha Chang", "TC"))

#Create subdstrict column
dog_density$subdistrict <- str_extract(dog_density$polygon, "^[A-Za-z]+") #extract the letters from the "polygon" column and make them a new column "subdistrict"

#Define subdistrict as categorical with 2 levels
dog_density <- dog_density %>%
  mutate(subdistrict = factor(subdistrict, levels = c("KK","TC"))) #make "subdistrict" a factor with the levels 'KK' and 'TC'

#Create survey identifier based on polygon and year of survey
dog_density <- dog_density %>%
  mutate(
    year = format(as.Date(Timestamp), "%Y"),  # create a new column isolating year
    survey = paste(polygon, year, sep = "_")  # create new column combining polygon and year to create a unique identifier for surveys
  )

#Make survey a factor variable  
dog_density <- dog_density %>%
  mutate(survey = as.factor(survey))

#Correct dates for TC 12_2024 (showing up as all observations on same day)
dog_density <- dog_density %>%
  mutate(Timestamp = if_else(
    survey == "TC 12_2024" & str_detect(Notes, "day 2"),
    Timestamp + days(1),
    Timestamp
  ))

#add mode of transport and track length from dog density to summary dataset by matching survey
no_resights_sightings <- no_resights_sightings %>%
  left_join(dog_density %>% select(Timestamp, survey,Track.Length, Mode.Transport), by = c("Timestamp", "survey"))

################################################################################

##DEFINE STERILIZATION EFFORT##

##Sort clinic data
#subset by districts of focus
KK_TC_Clinic <- raw_clinic %>%
  filter(Subdistrict %in% c("Khok Kurat", "Tha Chang"))

#Recognize date as a date
KK_TC_Clinic$date_admission <- as.Date(KK_TC_Clinic$date_admission)

#Rename subdistrict column and convert to either KK or TC
KK_TC_Clinic <- KK_TC_Clinic %>%
  rename("subdistrict" = "Subdistrict")

KK_TC_Clinic <- KK_TC_Clinic %>%
  mutate(subdistrict = dplyr::recode(
    subdistrict,
    "Khok Kurat" = "KK",
    "Tha Chang" = "TC"
  ))

################################################################################

##DEFINE STERILIZATION EFFORT##

no_resights_sightings <- no_resights_sightings %>%
  rowwise() %>%
  mutate(
    effort_all_time = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < Timestamp &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_1y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < Timestamp &
        KK_TC_Clinic$date_admission >= Timestamp - years(1) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_3y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < Timestamp - years(2) &
        KK_TC_Clinic$date_admission >= Timestamp - years(3) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_2y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < Timestamp - years(1) &
        KK_TC_Clinic$date_admission >= Timestamp - years(2) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_4y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < Timestamp - years(3) &
        KK_TC_Clinic$date_admission >= Timestamp - years(4) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    )
    
  ) %>%
  ungroup()

################################################################################

##Scale Effort variables by human population in Subdistrict##

#Create total sterilization effort by human population
no_resights_sightings <- no_resights_sightings %>%
  mutate(sc_total = case_when(
    subdistrict == "KK" ~ effort_all_time/3000,
    subdistrict == "TC" ~ effort_all_time/4938))

#Create total sterilization effort 1 year ago by human population
no_resights_sightings <- no_resights_sightings %>%
  mutate(sc_4y = case_when(
    subdistrict == "KK" ~ effort_4y_ago/3000,
    subdistrict == "TC" ~ effort_4y_ago/4938))

#Create total sterilization effort 3 years ago by human population
no_resights_sightings <- no_resights_sightings %>%
  mutate(sc_3y = case_when(
    subdistrict == "KK" ~ effort_3y_ago/3000,
    subdistrict == "TC" ~ effort_3y_ago/4938))

#Create total sterilization effort 2 years ago by human population
no_resights_sightings <- no_resights_sightings %>%
  mutate(sc_2y = case_when(
    subdistrict == "KK" ~ effort_2y_ago/3000,
    subdistrict == "TC" ~ effort_2y_ago/4938))

#Create total sterilization effort 1 year ago by human population
no_resights_sightings <- no_resights_sightings %>%
  mutate(sc_1y = case_when(
    subdistrict == "KK" ~ effort_1y_ago/3000,
    subdistrict == "TC" ~ effort_1y_ago/4938))

################################################################################

#Remove missing values
no_resights_sightings <- na.omit(no_resights_sightings) #one dog that doesn't have ownership info

################################################################################

##SAVE RDS FILE##

#sightings
saveRDS(no_resights_sightings, file = "Data/sightings_v3.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
