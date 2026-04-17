################################################################################
##                    CLINIC ROUGH WORK                                       ##
################################################################################
## Checking seasonality of pregnancies in clinic data and checking percentage ##
## of dogs owned/stray in clinic data.                                        ##
################################################################################

## LOAD LIBRARIES ##
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

################################################################################

## IMPORT CLINIC DATA ##

clinic_data <- readRDS("Data/FULL_clinic_data.rds", refhook = NULL)

################################################################################

## CHECK SEASONALITY OF PREGNANCIES ##

#how many pregnancies do I have in the data set
clinic_data %>%
  filter(preg_status == "yes_preg") %>%
  nrow() #16, doubt that's enough to see a seasonal pattern

#Get probability dog is pregnant
females <- clinic_data %>%
  filter(sex == "female") %>%
  mutate(
    year = lubridate::year(date_admission),
    month = lubridate::month(date_admission, label = TRUE)
  ) %>%
  group_by(year, month) %>%
  summarise(
    total_females = n(),
    pregnant = sum(preg_status == "yes_preg", na.rm = TRUE),
    prob_pregnant = pregnant / total_females,
    ci_low = prop.test(pregnant, total_females)$conf.int[1],
    ci_high = prop.test(pregnant, total_females)$conf.int[2],
    .groups = "drop"
  )

#make sure months are properly ordered factors
females$month <- factor(females$month, levels = month.abb)

#plot probability of being pregnant by month
ggplot(females, aes(x = month, y = prob_pregnant, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Probability a Female Dog is Pregnant per Month",
       x = "Month",
       y = "Probability") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +
#plot by month
clinic_data %>%
  filter(preg_status == "yes_preg") %>%
  group_by(month = lubridate::floor_date(date_admission, "month")) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = month, y = count)) +
  geom_line() +
  labs(title = "Number of Pregnant Dogs Admitted per Month",
       x = "Month",
       y = "Number of Pregnant Dogs") +
  geom_rect(aes(xmin = as.Date("2022-02-01"), #highlight when KK mobile clinic running
                xmax = as.Date("2022-03-31"),
                ymin = -Inf, ymax = Inf),
            fill = "blue", alpha = 0.03) +
  geom_rect(aes(xmin = as.Date("2023-11-01"), #highlight when TC mobile clinic running
                xmax = as.Date("2024-01-31"),
                ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.03) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

################################################################################

## CHECK PERCENTAGE OF DOGS OWNED/STRAY DURING MOBILE/STATIC CLINICS ##

##Create a new variable for clinic type
#where if the subdistrict is KK, clinic is mobile for any admission between Feb 1, 2022 and Mar 31, 2022
#and if the subdistrict is TC, clinic is mobile for any admission between Nov 1, 2023 and Jan 31, 2024. 
#otherwise, clinic is static.
clinic_data <- clinic_data %>%
  mutate(clinic_type = case_when(
    (subdistrict == "KK" & date_admission >= as.Date("2022-02-01") & date_admission <= as.Date("2022-03-31")) |
      (subdistrict == "TC" & date_admission >= as.Date("2023-11-01") & date_admission <= as.Date("2024-01-31")) ~ "mobile",
    TRUE ~ "static"
  ))

##Plot number of dogs by clinic type
clinic_data %>%
  group_by(clinic_type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = clinic_type, y = count, fill = clinic_type)) +
  geom_col() +
  labs(title = "Number of Dogs Admitted by Clinic Type",
       x = "Clinic Type",
       y = "Number of Dogs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("mobile" = "lightblue", "static" = "lightgreen")) +
  theme(legend.position = "none")

##Look at the distribution of dog ownership status by clinic type
clinic_data %>%
  group_by(clinic_type, ownership) %>%
  summarise(count = n()) %>%
  group_by(clinic_type) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ggplot(aes(x = clinic_type, y = percent, fill = ownership)) +
  geom_col(position = "dodge") +
  labs(title = "Percentage of Dogs Owned vs Stray by Clinic Type",
       x = "Clinic Type",
       y = "Percentage of Dogs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Get percentage of each ownership status for each clinic type
ownership_percentages <- clinic_data %>%
  group_by(clinic_type, ownership) %>%
  summarise(count = n()) %>%
  group_by(clinic_type) %>%
  mutate(percent = count / sum(count) * 100)

################################################################################

##Use clinic data raw instead##

##Import clinic data raw
clinic_data_raw <- read.csv("Data/FULL_clinic_data.csv")

#how many pregnancies do I have in the data set
clinic_data_raw %>%
  filter(preg_status == "yes_preg") %>%
  nrow() #596
#how many female dogs do I have in the data set
clinic_data_raw %>%
  filter(sex == "female") %>%
  nrow() #20686 

#Get probability dog is pregnant
females <- clinic_data_raw %>%
  filter(sex == "female") %>%
  mutate(
    year = lubridate::year(date_admission),
    month = lubridate::month(date_admission, label = TRUE)
  ) %>%
  group_by(year, month) %>%
  summarise(
    total_females = n(),
    pregnant = sum(preg_status == "yes_preg", na.rm = TRUE),
    prob_pregnant = pregnant / total_females,
    ci_low = prop.test(pregnant, total_females)$conf.int[1],
    ci_high = prop.test(pregnant, total_females)$conf.int[2],
    .groups = "drop"
  )

#make sure months are properly ordered factors
females$month <- factor(females$month, levels = month.abb)

#plot probability of being pregnant by month
ggplot(females, aes(x = month, y = prob_pregnant, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Probability a Female Dog is Pregnant per Month",
       x = "Month",
       y = "Probability") +
  theme_minimal() +
  facet_wrap(~year) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


#plot by month
clinic_data_raw %>%
  filter(preg_status == "yes_preg") %>%
  mutate(date_admission = as.Date(date_admission)) %>%
  group_by(month = lubridate::floor_date(date_admission, "month")) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = month, y = count)) +
  geom_line() +
  labs(title = "Number of Pregnant Dogs Admitted per Month",
       x = "Month",
       y = "Number of Pregnant Dogs") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#get the average number of pregnant dogs admitted per month across all years
clinic_data_raw %>%
  filter(preg_status == "yes_preg") %>%
  mutate(date_admission = as.Date(date_admission)) %>%
  group_by(month = lubridate::month(date_admission, label = TRUE)) %>%
  summarise(avg_count = mean(n()), .groups = "drop") %>%
  ggplot(aes(x = month, y = avg_count, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Number of Pregnant Dogs Admitted per Month",
       x = "Month",
       y = "Average Number of Pregnant Dogs") +
  theme_minimal() +
  scale_x_discrete(limits = month.abb) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#average number of pregnant dogs admitted per month across all years
clinic_data_raw %>%
  filter(preg_status == "yes_preg") %>%
  mutate(date_admission = as.Date(date_admission)) %>%
  group_by(month = lubridate::month(date_admission, label = TRUE)) %>%
  summarise(avg_count = mean(n()), .groups = "drop") %>%
  ggplot(aes(x = month, y = avg_count, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Number of Pregnant Dogs Admitted per Month",
       x = "Month",
       y = "Average Number of Pregnant Dogs") +
  theme_minimal() +
  scale_x_discrete(limits = month.abb) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#Create summary dataset of mean number of females and  probability that they are pregnant by month
avg_females <- clinic_data_raw %>%
  mutate(
    date_admission = as.Date(date_admission),
    year = year(date_admission),
    month = month(date_admission, label = TRUE)
  ) %>%
  group_by(year, month) %>% #summarize per year month
  summarise(
    n_fem = sum(sex == "female", na.rm = TRUE),
    n_preg = sum(sex == "female" & preg_status == "yes_preg", na.rm = TRUE),
    prop_preg = n_preg / n_fem,
    .groups = "drop"
  ) %>%
  group_by(month) %>% #summarize across years
  summarise(
    avg_fem = mean(n_fem, na.rm = TRUE),
    prob_preg = mean(prop_preg, na.rm = TRUE),
    se = sd(prop_preg, na.rm = TRUE) / sqrt(n()), #with standard error
    ci_low = prob_preg - 1.96 * se, #with 95% CI
    ci_high = prob_preg + 1.96 * se,
    .groups = "drop"
  )

#plot probability of being pregnant by month
ggplot(avg_females, aes(x = month, y = prob_preg, group = 1)) +
  geom_point(data = females,
             aes(x = month, y = prob_pregnant),
             inherit.aes = FALSE,
             alpha = 0.4,
             size = 2,
             color = "grey40",
             position = position_jitter(width = 0.15, height = 0)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Probability a Female Dog is Pregnant per Month",
       x = "Month",
       y = "Probability") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +

#average percentage of female dogs that are pregnant per month across all years
clinic_data_raw %>%
  filter(sex == "female") %>%
  mutate(date_admission = as.Date(date_admission)) %>%
  group_by(month = lubridate::month(date_admission, label = TRUE)) %>%
  summarise(pregnant = mean(preg_status == "yes_preg", na.rm = TRUE) * 100, .groups = "drop") %>%
  ggplot(aes(x = month, y = pregnant, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Percentage of Female Dogs that are Pregnant per Month",
       x = "Month",
       y = "Average Percentage of Pregnant Dogs") +
  theme_minimal() +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


