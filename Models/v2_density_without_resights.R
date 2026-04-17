################################################################################
##             Removing Re-sights and Testing Dog Density                     ##
################################################################################
# Redoing model 1 - dog density but removing re-sights                         #
################################################################################
# Created Mar.23, 2026 by Bronte Slote, last edited Mar. 23, 2026              #
################################################################################

##LOAD LIBRARIES##
library(readr) #reading csv files
library(dplyr) #organizing and manipulating data
library(lubridate) #formatting dates and times
library(ggplot2) #creating plots
library(stringr) #manipulating text
library(lme4) #creating mixed models with random effect
library(RVAideMemoire) #checking for overdispersion
library(DHARMa) #checking overdispersion visually
library(ggeffects) #creating predicted values and visualizing them
library(lmtest) # for likelihood ratio tests
library(car) # for variance inflation factors
library(corrplot) # to check for correlation between EVs
library(patchwork) #combine graphs into 1 panel

################################################################################

##IMPORT DATA##

dog_density <- readRDS("Data/FULL_dog_density.rds", refhook = NULL)
sightings <- readRDS("Data/FULL_sightings.rds", refhook = NULL)
clinic <- readRDS("Data/FULL_clinic_data.rds", refhook = NULL)

################################################################################

##CREATE MY OWN SUMMARY DATASET##

##summarize sightings by survey
#only want to sum - Adult.male, Adult.NON.lactating.female, Adult.Lactating.female, 
# Adult.unknown.sex, Puppy, Entire, Neutered, Unknown, Free.roaming.collared,
# Free.roaming.NO.collar, Confined.in.yard, On.chain.or.lead, Inside.house,
# Healthy, Sick.or.injured, Unknown.health, Skin.disease, Wound, Underweight.or.thin,
# TVT, Mange, Lameness...facture, Other 
summary <- sightings %>%
  group_by(survey,year, date, day, subdistrict, polygon) %>%
  summarise(
    n_obs = n(),
    Adult.male = sum(Adult.male, na.rm = TRUE),
    Adult.NON.lactating.female = sum(Adult.NON.lactating.female, na.rm = TRUE),
    Adult.Lactating.female = sum(Adult.Lactating.female, na.rm = TRUE),
    Adult.unknown.sex = sum(Adult.unknown.sex, na.rm = TRUE),
    Puppy = sum(Puppy, na.rm = TRUE),
    Entire = sum(Entire, na.rm = TRUE),
    Neutered = sum(Neutered, na.rm = TRUE),
    Unknown = sum(Unknown, na.rm = TRUE),
    Free.roaming.collared = sum(Free.roaming.collared, na.rm = TRUE),
    Free.roaming.NO.collar = sum(Free.roaming.NO.collar, na.rm = TRUE),
    Confined.in.yard = sum(Confined.in.yard, na.rm = TRUE),
    On.chain.or.lead = sum(On.chain.or.lead, na.rm = TRUE),
    Inside.house = sum(Inside.house, na.rm = TRUE),
    Healthy = sum(Healthy, na.rm = TRUE),
    Sick.or.injured = sum(Sick.or.injured, na.rm = TRUE),
    Unknown.health = sum(Unknown.health, na.rm = TRUE),
    Skin.disease = sum(Skin.disease, na.rm = TRUE),
    Wound = sum(Wound, na.rm = TRUE),
    Underweight.or.thin = sum(Underweight.or.thin, na.rm = TRUE),
    TVT = sum(TVT, na.rm = TRUE),
    Mange = sum(Mange, na.rm = TRUE),
    Lameness...facture = sum(Lameness...facture, na.rm = TRUE),
    Other = sum(Other, na.rm = TRUE)
  )

################################################################################

## CREATE EFFORT VARIABLES - TIME SINCE INTERVENTION ##

#Create date since intervention column
summary <- summary %>%
  mutate(
    intervention_start = case_when( #create column "intervention_start"
      subdistrict == "KK" ~ as.Date("2022-02-11"), #where the subdistrict is "KK" make the intervention start date as 2022-02-11
      subdistrict == "TC" ~ as.Date("2023-11-17") #where the subdistrict is "TC" make the intervention start date as 2022-11-17
    ),
    since_intervention = as.numeric(date - intervention_start) #create a new numeric column "since_intervention" by subtracting intervention start date from date of survey resulting ina column showing number of days since intervention
  )

#Make days since intervention to years
summary <- summary %>%
  mutate(since_intervention = since_intervention / 365)

################################################################################

## CREATE EFFORT VARIABLES - TOTAL & ANNUAL EFFORT ##

summary <- summary %>%
  rowwise() %>%
  mutate(
    effort_all_time = sum(
      as.character(clinic$subdistrict) == subdistrict &
        clinic$date_admission < date &
        (grepl("castration",clinic$type_surgery)|grepl("spay",clinic$type_surgery))
    ),
    effort_1y_ago = sum(
      as.character(clinic$subdistrict) == subdistrict &
        clinic$date_admission < date &
        clinic$date_admission >= date - years(1) &
        (grepl("castration",clinic$type_surgery)|grepl("spay",clinic$type_surgery))
    ),
    effort_3y_ago = sum(
      as.character(clinic$subdistrict) == subdistrict &
        clinic$date_admission < date - years(2) &
        clinic$date_admission >= date - years(3) &
        (grepl("castration",clinic$type_surgery)|grepl("spay",clinic$type_surgery))
    ),
    effort_2y_ago = sum(
      as.character(clinic$subdistrict) == subdistrict &
        clinic$date_admission < date - years(1) &
        clinic$date_admission >= date - years(2) &
        (grepl("castration",clinic$type_surgery)|grepl("spay",clinic$type_surgery))
    ),
    effort_4y_ago = sum(
      as.character(clinic$subdistrict) == subdistrict &
        clinic$date_admission < date - years(3) &
        clinic$date_admission >= date - years(4) &
        (grepl("castration",clinic$type_surgery)|grepl("spay",clinic$type_surgery))
    )
    
  ) %>%
  ungroup()

################################################################################

##ADD MODE OF TRANSPORT AND TRACK LENGTH FROM DOG DENSITY##

#add mode of transport and track length from dog density to summary dataset by matching survey
summary <- summary %>%
  left_join(dog_density %>% select(date, survey,Track.Length, Mode.Transport), by = c("date", "survey"))

################################################################################

                          ## RERUNNING MODEL 1 ##

################################################################################

##SINCE INTERVENTION##

#most complex model
m1_since_intervention <- glmer(n_obs ~ since_intervention + subdistrict + day + Mode.Transport +
                                 (1 | polygon) +
                                 offset(log(Track.Length)), 
                               family = poisson, data = summary, control=glmerControl(optimizer="bobyqa"))
summary(m1_since_intervention)
#check vif
vif(m1_since_intervention)#all good
#check what to drop
drop1(m1_since_intervention, test = "Chisq")#all significant

################################################################################

##TOTAL EFFORT##

#most complex model
m1_effort <- glmer(n_obs ~ effort_all_time + subdistrict + day + Mode.Transport +
                              (1 | polygon) +
                              offset(log(Track.Length)), 
                            family = poisson, data = summary, control=glmerControl(optimizer="bobyqa"))
#model failed to converge - probably because I didn't scale total effort
#created scaled effort_all_time
summary$effort_scaled <- scale(summary$effort_all_time)
#save scaling parameters
effort_mean <- mean(summary$effort_all_time, na.rm = TRUE)
effort_sd   <- sd(summary$effort_all_time, na.rm = TRUE)

#updated model
m1_effort <- glmer(n_obs ~ effort_scaled + subdistrict + day + Mode.Transport +
                              (1 | polygon) +
                              offset(log(Track.Length)), 
                            family = poisson, data = summary, control=glmerControl(optimizer="bobyqa"))
#check vif
vif(m1_effort)#all good
#check what to drop
drop1(m1_effort, test = "Chisq")#all significant

################################################################################

##ANNUAL EFFORT##

#check for correlation between year variables
years_effort <- summary %>% 
  select(effort_4y_ago, effort_3y_ago,
         effort_2y_ago, effort_1y_ago)

corrplot(cor(years_effort, use = "pairwise.complete.obs"),
         method = "color",
         type = "upper",
         tl.col = "black") #No correlation between years
#all good no correlation between years, can include all in model

#most complex model
m1_year <- glmer(n_obs ~ effort_4y_ago + effort_3y_ago + effort_2y_ago + effort_1y_ago + subdistrict + day + Mode.Transport +
                   (1 | polygon) +
                   offset(log(Track.Length)), 
                 family = poisson, data = summary,control=glmerControl(optimizer="bobyqa"))
#updated model - won't converge need to scale years
#created scaled effort_all_time
summary$y1_scaled <- scale(summary$effort_1y_ago)
summary$y2_scaled <- scale(summary$effort_2y_ago)
summary$y3_scaled <- scale(summary$effort_3y_ago)
summary$y4_scaled <- scale(summary$effort_4y_ago)
#save scaling parameters
y1_mean <- mean(summary$effort_1y_ago, na.rm = TRUE)
y1_sd   <- sd(dog_density$effort_1y_ago, na.rm = TRUE)
y2_mean <- mean(summary$effort_2y_ago, na.rm = TRUE)
y2_sd   <- sd(dog_density$effort_2y_ago, na.rm = TRUE)
y3_mean <- mean(summary$effort_3y_ago, na.rm = TRUE)
y3_sd   <- sd(dog_density$effort_3y_ago, na.rm = TRUE)
y4_mean <- mean(summary$effort_4y_ago, na.rm = TRUE)
y4_sd   <- sd(dog_density$effort_4y_ago, na.rm = TRUE)

#updated model
m1_year <- glmer(n_obs ~ y4_scaled + y3_scaled + y2_scaled + y1_scaled + subdistrict + day + Mode.Transport +
                   (1 | polygon) +
                   offset(log(Track.Length)), 
                 family = poisson, data = summary,control=glmerControl(optimizer="bobyqa"))
#check vif
vif(m1_year)#all good

#check what to drop
drop1(m1_year, test = "Chisq")#drop effort 3 year ago

#updated model dropping 3 years ago
m1.1_year <- glmer(n_obs ~ y4_scaled + y2_scaled + y1_scaled + subdistrict + day + Mode.Transport +
                   (1 | polygon) +
                   offset(log(Track.Length)), 
                 family = poisson, data = summary,control=glmerControl(optimizer="bobyqa"))
#check what to drop
drop1(m1.1_year, test = "Chisq")#drop effort 4 years ago

#updated model dropping 4 years ago
m1.2_year <- glmer(n_obs ~ y2_scaled + y1_scaled + subdistrict + day + Mode.Transport +
                   (1 | polygon) +
                   offset(log(Track.Length)), 
                 family = poisson, data = summary,control=glmerControl(optimizer="bobyqa"))
#check what to drop
drop1(m1.2_year, test = "Chisq")#drop effort 1 year ago

#updated model dropping 1 year ago
m1.3_year <- glmer(n_obs ~ y2_scaled + subdistrict + day + Mode.Transport +
                   (1 | polygon) +
                   offset(log(Track.Length)), 
                 family = poisson, data = summary,control=glmerControl(optimizer="bobyqa"))
#check what to drop
drop1(m1.3_year, test = "Chisq")#all significant - final model

################################################################################

## COMPARE MODELS ##
AIC(m1_since_intervention, m1_effort, m1.3_year) #m1.3_year has lowest AIC, so best model

################################################################################

##CHECK OVERDISPERSION##

#for m1_since_intervention
overdisp.glmer(m1_since_intervention) 
simulationOutput_m1_since <- simulateResiduals(fittedModel = m1_since_intervention) #create simulated data
testDispersion(simulationOutput_m1_since)
testOutliers(simulationOutput_m1_since)
testZeroInflation(simulationOutput_m1_since)
testUniformity(simulationOutput_m1_since)
plot(simulationOutput_m1_since)

#for m1_effort
overdisp.glmer(m1_effort)
simulationOutput_m1_effort <- simulateResiduals(fittedModel = m1_effort)
testDispersion(simulationOutput_m1_effort)
testOutliers(simulationOutput_m1_effort)
testZeroInflation(simulationOutput_m1_effort)
testUniformity(simulationOutput_m1_effort)
plot(simulationOutput_m1_effort)

#for m1.3_year
overdisp.glmer(m1.3_year)
simulationOutput_m1_year <- simulateResiduals(fittedModel = m1.3_year)
testDispersion(simulationOutput_m1_year)
testOutliers(simulationOutput_m1_year)
testZeroInflation(simulationOutput_m1_year)
testUniformity(simulationOutput_m1_year)
plot(simulationOutput_m1_year)

################################################################################

## PLOT EACH MODEL ##

#plot predicted values for m1_since_intervention
preds1_since <- ggpredict(m1_since_intervention,
                          terms = c("since_intervention", "subdistrict"),
                          condition = c(Track.Length = 1),
                          type = "fixed"
)
ggplot(preds1_since, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_point() +
  labs(title = "Predicted Dog Density by Time Since Intervention",
       x = "Years Since Intervention",
       y = "Predicted Dog Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("KK" = "blue", "TC" = "red"), name = "Subdistrict")

plot(preds1_since) +
  labs(title = "Predicted Sightings/km by\n Time Since Intervention\n and Subdistrict",
       x = "Time Since Intervention (Years)",
       y = "Predicted Sightings per Km") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    axis.text = element_text(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 22))

################################################################################

#plot predicted values for m1_effort

#get predicted values for m1_effort 
preds1_effort <- ggpredict(m1_effort,
                          terms = c("effort_scaled", "subdistrict"),
                          condition = c(Track.Length = 1),
                          type = "fixed"
)
#unscale x values for plotting
preds1_effort$x <- preds1_effort$x * effort_sd + effort_mean

#plot
ggplot(preds1_effort, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_point() +
  labs(title = "Predicted Dog Density by Total Sterilization Effort",
       x = "Total Sterilization Effort",
       y = "Predicted Dog Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("KK" = "blue", "TC" = "red"), name = "Subdistrict")


#plot predicted values for m1.3_year

#get predicted values for m1.3_year
preds1_year <- ggpredict(m1.3_year,
                          terms = c("y2_scaled", "subdistrict"),
                          condition = c(Track.Length = 1),
                          type = "fixed"
)

#unscale x values for plotting
preds1_year$x <- preds1_year$x * y2_sd + y2_mean

#plot
ggplot(preds1_year, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_point() +
  labs(title = "Predicted Dog Density by Sterilization Effort 2 Years Ago",
       x = "Sterilization Effort 2 Years Ago",
       y = "Predicted Dog Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("KK" = "blue", "TC" = "red"), name = "Subdistrict") 



