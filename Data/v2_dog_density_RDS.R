###############################################################################
##                   v2 Dog Density RDS                                      ##
###############################################################################
## Final version of cleaned and organized dog density and clinic data sets   ##
## with FULL updated dataset including most recent surveys/sterilizations    ##
## as of April 2026. To be used v3 dog density model.                        ##
###############################################################################
## Created Oct. 15, 2025 by Bronte Slote, last updated Apr. 24, 2026         ##
###############################################################################

##LOAD LIBRARIES##

library(readr) #reading csv files
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(stringr) #manipulating text

################################################################################


##IMPORTING DATA ##

#Import most up to date dog density dataset
dog_densityRAW <- read.csv("Data/FULL_dog_density.csv")

#Rename columns
dog_density <- dog_densityRAW %>% rename("polygon" = "Sandbox.Name", #rename so easier to remember
                                      "date"= "Timestamp")

################################################################################

##ORGANIZE POLYGONS AND SUBDISTRICTS##

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

################################################################################

##ORGANIZE DATES##

#Recognize date as a date
dog_density$date <- parse_date_time(dog_density$date, orders = "d B y, H:M")

#Convert date to date formatting
dog_density$date <- as.Date(dog_density$date)

#Check for duplicates
dup_rows <- dog_density[dog_density$date %in% dog_density$date[duplicated(dog_density$date)], ]

#Time zone error in row 25 causing to appear as duplicate day survey, add 1 day to time
dog_density$date[25] <- dog_density$date[25] + days(2)

################################################################################

##CREATE SINCE INTERVENTION VARIABLE##

#Create date since intervention column
dog_density <- dog_density %>%
  mutate(
    intervention_start = case_when( #create column "intervention_start"
      subdistrict == "KK" ~ as.Date("2022-02-11"), #where the subdistrict is "KK" make the intervention start date as 2022-02-11
      subdistrict == "TC" ~ as.Date("2023-11-17") #where the subdistrict is "TC" make the intervention start date as 2022-11-17
    ),
    since_intervention = as.numeric(date - intervention_start) #create a new numeric column "since_intervention" by subtracting intervention start date from date of survey resulting ina column showing number of days since intervention
  )

#Make days since intervention to years
dog_density <- dog_density %>%
  mutate(since_intervention = since_intervention / 365)

################################################################################

##SURVEY IDENTIFIER VARIABLE##

#Create survey identifier based on polygon and year of survey
dog_density <- dog_density %>%
  mutate( 
    year = format(as.Date(date), "%Y"),  # create a new column isolating year
    survey = paste(polygon, year, sep = "_")  # create new column combining polygon and year to create a unique identifier for surveys
  )

#Make survey a factor variable  
dog_density <- dog_density %>%
  mutate(survey = as.factor(survey))

################################################################################

##CREATE DAY VARIABLE##

#Create variable "day" to show either day 1 or 2 survey
dog_density <- dog_density %>%
  group_by(survey) %>%
  mutate(day = dense_rank(date)) %>%
  ungroup()

#Make day a factor variable  
dog_density <- dog_density %>%
  mutate(day= as.factor(day))

################################################################################

##FORMAT TRACK LENGTH, MODE OF TRANSPORT, AND OWNERSHIP STATUS##

#Make track length into km instead of meters
dog_density<-dog_density %>%
  mutate(Track.Length = Track.Length/1000)

#Create new column "Mode.Transport"
dog_density$Mode.Transport <- str_extract(
  dog_density$Notes,
  "walking|4-wheeler|2-wheeler|bicycle"
)

#Make any NAs be shown as unknown
dog_density <- dog_density %>%
  mutate(Mode.Transport = if_else(is.na(Mode.Transport), "Unknown", Mode.Transport))

#Make "Mode.Transport" categorical
dog_density <- dog_density %>%
  mutate(Mode.Transport= as.factor(Mode.Transport))

#Create new column owned
dog_density <- dog_density %>%
  mutate(Owned = Free.roaming.collared + Confined.in.yard + On.chain.or.lead)

################################################################################

##DEFINE STERILIZATION EFFORT##

#Import data set "clinic data"
clinic_dataRAW <- read.csv("Data/FULL_clinic_data.csv")

#Define "subdistrict" as factor
clinic_data <- clinic_dataRAW %>%
  mutate(Subdistrict = as.factor(Subdistrict))

#Create new data set with only regions of "Khok Kurat" and "Tha Chang"
KK_TC_Clinic <- clinic_data %>%
  filter(Subdistrict %in% c("Khok Kurat", "Tha Chang"))

################################################################################

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
dog_density <- dog_density %>%
  rowwise() %>%
  mutate(
    effort_all_time = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_1y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date &
        KK_TC_Clinic$date_admission >= date - years(1) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_3y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date - years(2) &
        KK_TC_Clinic$date_admission >= date - years(3) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_2y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date - years(1) &
        KK_TC_Clinic$date_admission >= date - years(2) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    ),
    effort_4y_ago = sum(
      as.character(KK_TC_Clinic$subdistrict) == subdistrict &
        KK_TC_Clinic$date_admission < date - years(3) &
        KK_TC_Clinic$date_admission >= date - years(4) &
        (grepl("castration",KK_TC_Clinic$type_surgery)|grepl("spay",KK_TC_Clinic$type_surgery))
    )
    
  ) %>%
  ungroup()

#Create total sterilization effort by human population
dog_density <- dog_density %>%
  mutate(effort_humanpop = case_when(
    subdistrict == "KK" ~ effort_all_time/3000,
    subdistrict == "TC" ~ effort_all_time/4938))

#Create total sterilization effort 1 year ago by human population
dog_density <- dog_density %>%
  mutate(effort_4y_humanpop = case_when(
    subdistrict == "KK" ~ effort_4y_ago/3000,
    subdistrict == "TC" ~ effort_4y_ago/4938))

#Create total sterilization effort 3 years ago by human population
dog_density <- dog_density %>%
  mutate(effort_3y_humanpop = case_when(
    subdistrict == "KK" ~ effort_3y_ago/3000,
    subdistrict == "TC" ~ effort_3y_ago/4938))

#Create total sterilization effort 2 years ago by human population
dog_density <- dog_density %>%
  mutate(effort_2y_humanpop = case_when(
    subdistrict == "KK" ~ effort_2y_ago/3000,
    subdistrict == "TC" ~ effort_2y_ago/4938))

#Create total sterilization effort 1 year ago by human population
dog_density <- dog_density %>%
  mutate(effort_1y_humanpop = case_when(
    subdistrict == "KK" ~ effort_1y_ago/3000,
    subdistrict == "TC" ~ effort_1y_ago/4938))

################################################################################

##Save as objects

#dog_density
saveRDS(dog_density, file = "Data/density_v2.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

#refined clinic data with only KK_TC_Clinic
saveRDS(KK_TC_Clinic, file = "Data/clinic_data_v2.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
