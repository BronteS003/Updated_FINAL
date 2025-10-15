###############################################################################
##                   FULL Dog Density RDS                                    ##
###############################################################################
## Final version of cleaned and organized dog density and clinic data sets   ##
## with FULL updated dataset including most recent surveys/sterilizations    ##
## as of October 2025. To be used in all FULL models.                        ##
###############################################################################
## Created Oct. 15, 2025 by Bronte Slote, last updated Oct. 15               ##
###############################################################################

##LOAD LIBRARIES##

library(readr) #reading csv files
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(stringr) #manipulating text


##IMPORTING & CLEANING DATA ##

#Import most up to date dog density dataset
dog_density <- read.csv("FULL_dog_density.csv")

#Rename columns
dog_density <- dog_density %>% rename("polygon" = "Sandbox.Name", #rename so easier to remember
                                      "date"= "Timestamp")

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
