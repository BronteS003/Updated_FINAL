################################################################################
#             Sterilizations Overtime                                          #
################################################################################
# Plotting clinic data overtime to visualize the number of sterilizations      #
# conducted overtime.                                                          #
################################################################################
# Created Aug. 19, 2025 by Bronte Slote, last modified Aug. 21, 2025           #
################################################################################

##Import clinic data
clinic_data <- readRDS("clinic.rds", refhook = NULL)

##Load Libraries
library(ggplot2)
library(dplyr)
library(lubridate)

##Clean data

##Remove other surgeries
clinic_data <- clinic_data %>%
  filter(type_surgery != "Other")

#Format admission date as a date
clinic_data$date_admission <- as.Date(clinic_data$date_admission)

# Extract year-month
clinic_data <- clinic_data %>%
  mutate(month = floor_date(date_admission, "month")) %>% 
  group_by(month, subdistrict) %>%
  summarise(surgery_counts = n(), .groups = "drop")

#Plot
ggplot(clinic_data, aes(x = month, y = surgery_counts, fill = subdistrict)) +
  geom_col(
    position = position_dodge2(width = 25, preserve = "single"),
    width = 20                                                   
  ) +
  labs(title = "Number of Sterilization Conducted per Month",
       x = "Month",
       y = "Number of Surgeries") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

