################################################################################
##                     Dog Density (RDS version)                              ##
################################################################################
## Final version of cleaned and organized dog density, clinic_data, and       ##
## KK_TC_Clinic files, to be used in all models going forward                 ##
################################################################################
## Created July 23, 2025 by Bronte Slote, lasted edited Aug. 21, 2025         ##
################################################################################


##Load Libraries
library(readr) #reading csv files
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(stringr) #manipulating text

##IMPORTING & CLEANING DATA ##

#Import data set "dog_density"
dog_density <- read.csv("final_dog_density.csv") #import data set and name it "dog_density"
# View(dog_density)#view data set

#Rename columns
dog_density <- dog_density %>% rename("polygon" = "Sandbox.Name", #rename so easier to remember
                                      "date"= "Timestamp")

#Recognize polygon as a categorical variable with 7 levels
dog_density <- dog_density %>%
  mutate(polygon = factor (polygon, levels = c("KK 01","KK 06","KK 07", "TC 12", "TC 16","TC 20","TC 24")))

#Create subdstrict column
dog_density$subdistrict <- str_extract(dog_density$polygon, "^[A-Za-z]+") #extract the letters from the "polygon" column and make them a new column "subdistrict"

#Define subdistrict as categorical with 2 levels
dog_density <- dog_density %>%
  mutate(subdistrict = factor(subdistrict, levels = c("KK","TC"))) #make "subdistrict" a factor with the levels 'KK' and 'TC'

#Recognize date as a date
dog_density$date <- parse_date_time(dog_density$date, orders = "d B y, H:M")

#Convert date to date formatting
dog_density$date <- as.Date(dog_density$date)

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

#Remove On.Route column because its values are identical to Sighting.Count column
dog_density$On.Route <- NULL

#Create survey identifier based on polygon and year of survey
dog_density <- dog_density %>%
  mutate( 
    year = format(as.Date(date), "%Y"),  # create a new column isolating year
    survey = paste(polygon, year, sep = "_")  # create new column combining polygon and year to create a unique identifier for surveys
  )

#Make survey a factor variable  
dog_density <- dog_density %>%
  mutate(survey = as.factor(survey))

#Create variable "day" to show either day 1 or 2 survey
dog_density <- dog_density %>%
  group_by(polygon, year) %>%                         # group by subdistrict and year
  arrange(date, .by_group = TRUE) %>%                     # sort by date *within* each group
  mutate(day = row_number()) %>%                          # assign 1 to earliest date, 2 to second day
  ungroup()

#Make day a factor variable  
dog_density <- dog_density %>%
  mutate(day= as.factor(day))

#Make track length into km instead of meters
dog_density<-dog_density %>%
  mutate(Track.Length = Track.Length/1000)

#Create new column "Mode.Transport"
dog_density$Mode.Transport <- str_extract(
  dog_density$Notes,
  "walking|4-wheeler|2-wheeler|bicycle"
)

#Make "Mode.Transport" categorical
dog_density <- dog_density %>%
  mutate(Mode.Transport= as.factor(Mode.Transport))

#Create new column owned
dog_density <- dog_density %>%
  mutate(Owned = Free.roaming.collared + Confined.in.yard + On.chain.or.lead)


##DEFINE STERILIZATION EFFORT##

#Import data set "clinic data"
clinic_data <- read_csv("clinic_data .csv")
# View(clinic_data)

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
    )
    
  ) %>%
  ungroup()

#Create total sterilization effort by human population
dog_density <- dog_density %>%
  mutate(effort_humanpop = case_when(
    subdistrict == "KK" ~ effort_all_time/3000,
    subdistrict == "TC" ~ effort_all_time/4938))

#Create total sterilization effort last 3 years by human population
dog_density <- dog_density %>%
  mutate(effort_3y_humanpop = case_when(
    subdistrict == "KK" ~ effort_3y_ago/3000,
    subdistrict == "TC" ~ effort_3y_ago/4938))

#Create total sterilization effort last 2 years by human population
dog_density <- dog_density %>%
  mutate(effort_2y_humanpop = case_when(
    subdistrict == "KK" ~ effort_2y_ago/3000,
    subdistrict == "TC" ~ effort_2y_ago/4938))

#Create total sterilization effort last 1 year by human population
dog_density <- dog_density %>%
  mutate(effort_1y_humanpop = case_when(
    subdistrict == "KK" ~ effort_1y_ago/3000,
    subdistrict == "TC" ~ effort_1y_ago/4938))




##Save as objects

#dog_density
saveRDS(dog_density, file = "dog_density.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

#refined clinic data with only KK_TC_Clinic
saveRDS(KK_TC_Clinic, file = "clinic.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
