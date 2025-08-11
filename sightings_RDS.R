################################################################################
##                     Sightings (RDS version)                              ##
################################################################################
## Final version of cleaned and organized sightings file, to be used in all   ##
## models going forward                                                       ##
################################################################################
## Created July 26, 2025 by Bronte Slote, lasted edited July 26, 2025         ##
################################################################################

##Load Libraries
library(readr) #reading csv files
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(stringr) #manipulating text



##IMPORT AND CLEAN DATA##

##Import Data set "sightings"
sightings <- read.csv("final_sightings.csv")


#Remove all rows that are resights
sightings <- sightings %>%
  filter(!str_to_lower(Notes) %in% c("resight", "unknown"))


##Rename columns
sightings <- sightings %>% rename("polygon" = "Sandbox.Name",#rename so easier to remember
                                  "date"= "Timestamp")

#Recognize polygon as a categorical variable with 7 levels
sightings <- sightings %>%
  mutate(polygon = factor (polygon, levels = c("KK 01","KK 06","KK 07", "TC 12", "TC 16","TC 20","TC 24")))

#Define column Neutered as numerical
sightings <- sightings %>%
  mutate(Neutered = as.numeric(Neutered)) #make "Neutered" numerical

#Create subdstrict column
sightings$subdistrict <- str_extract(sightings$polygon, "^[A-Za-z]+") #extract the letters from the "polygon" column and make them a new column "subdistrict"

#Define subdistrict as categorical with 2 levels
sightings <- sightings %>%
  mutate(subdistrict = factor(subdistrict, levels = c("KK","TC"))) #make "subdistrict" a factor with the levels 'KK' and 'TC'

#Convert date to a date variable
sightings$date <- as_date(ymd_hms(sightings$date)) #convert text date of year-month-day-hour-minute-seconds to date only, removing time

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

#Create survey identifier based on polygon and year of survey
sightings <- sightings %>%
  mutate(
    year = format(as.Date(date), "%Y"),  # create a new column isolating year
    survey = paste(polygon, year, sep = "_")  # create new column combining polygon and year to create a unique identifier for surveys
  )

#Make survey a factor variable  
sightings <- sightings %>%
  mutate(survey = as.factor(survey))

#Create variable "day" to show either day 1 or 2 survey
sightings <- sightings %>%
  mutate(day = case_when( #create new column "day"
    polygon == "KK 01" & date %in% as.Date(c("2021-10-06", "2023-02-28", "2024-08-26")) ~ 1,
    polygon == "KK 06" & date %in% as.Date(c("2021-10-07", "2023-02-28", "2024-08-26")) ~ 1,
    polygon == "KK 07" & date %in% as.Date(c("2021-10-06", "2023-02-27", "2024-08-26")) ~ 1,
    polygon == "TC 12" & date %in% as.Date(c("2023-03-16", "2024-09-09")) ~ 1,
    polygon == "TC 16" & date %in% as.Date(c("2023-03-17", "2024-09-09")) ~ 1,
    polygon == "TC 20" & date %in% as.Date(c("2023-03-17", "2024-09-09")) ~ 1,
    polygon == "Tc 24" & date %in% as.Date(c("2023-03-17", "2024-09-10")) ~ 1,
    TRUE ~ 2  # everything else gets "2"
  ))

#Make day a factor variable  
sightings <- sightings %>%
  mutate(day= as.factor(day))

#Make column age
sightings <- sightings %>%
  mutate(age = case_when(
    Adult.male == 1 ~ "Adult",
    Adult.NON.lactating.female == 1 | Adult.Lactating.female == 1 ~ "Adult",
    Adult.unknown.sex == 1 ~ "Adult",
    Puppy == 1 ~ "Puppy"))

#Make age a factor
sightings<-sightings %>%
  mutate(age = as.factor(age))

#Make column sex
sightings <- sightings %>%
  mutate(sex = case_when(
    Adult.male == 1 ~ "M",
    Adult.NON.lactating.female == 1 | Adult.Lactating.female == 1 ~ "F",
    Adult.unknown.sex == 1|Puppy ==1 ~ "Unknown"))

#Make sex a factor
sightings<-sightings %>%
  mutate(sex = as.factor(sex))

#Make column owned
sightings <- sightings %>%
  mutate(owned = case_when(
    Free.roaming.NO.collar == 1 ~ "No",
    Free.roaming.collared | Confined.in.yard | On.chain.or.lead == 1 ~ "Yes"))

#Make owned a factor
sightings<-sightings %>%
  mutate(owned = as.factor(owned))

#Remove NAs
sightings <- na.omit(sightings)

##DEFINE STERILIZATION EFFORT##

#Import data set "clinic data"
clinic_data <- read_csv("clinic_data .csv")
View(clinic_data)

#Define "subdistrict" as factor
clinic_data <- clinic_data %>%
  mutate(Subdistrict = as.factor(Subdistrict))

#Create new data set with only regions of "Khok Kurat" and "Tha Chang"
KK_TC_Clinic <- clinic_data %>%
  filter(Subdistrict %in% c("Khok Kurat", "Tha Chang"))

##Define variables in clinic_data data set

#Define "Year" as factor
KK_TC_Clinic <- KK_TC_Clinic %>%
  mutate(Year = as.factor(Year))

#Define "age" as factor
KK_TC_Clinic <- KK_TC_Clinic %>%
  mutate(age = as.factor(age))

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

#Define sterilization effort
sightings <- sightings %>%
  rowwise() %>%
  mutate(
    effort_all_time = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_last_3y = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date &
        KK_TC_Clinic$date_admission >= date - years(3) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_last_2y = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date &
        KK_TC_Clinic$date_admission >= date - years(2) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_last_1y = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date &
        KK_TC_Clinic$date_admission >= date - years(1) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    )
  ) %>%
  ungroup()

#Create total sterilization effort by human population
sightings <- sightings %>%
  mutate(effort_humanpop = case_when(
    subdistrict == "KK" ~ effort_all_time/3000,
    subdistrict == "TC" ~ effort_all_time/4938))

#Create total sterilization effort last 3 years by human population
sightings <- sightings %>%
  mutate(last3y_humanpop = case_when(
    subdistrict == "KK" ~ effort_last_3y/3000,
    subdistrict == "TC" ~ effort_last_3y/4938))

#Create total sterilization effort last 2 years by human population
sightings <- sightings %>%
  mutate(last2y_humanpop = case_when(
    subdistrict == "KK" ~ effort_last_2y/3000,
    subdistrict == "TC" ~ effort_last_2y/4938))

#Create total sterilization effort last 1 year by human population
sightings <- sightings %>%
  mutate(last1y_humanpop = case_when(
    subdistrict == "KK" ~ effort_last_1y/3000,
    subdistrict == "TC" ~ effort_last_1y/4938))

#Save RDS

#sightings
saveRDS(sightings, file = "sightings.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
