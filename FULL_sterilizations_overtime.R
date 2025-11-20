################################################################################
#             FULL Sterilizations Overtime                                     #
################################################################################
# Plotting clinic data overtime to visualize the number of sterilizations      #
# conducted overtime.                                                          #
################################################################################
# Created Oct. 15, 2025 by Bronte Slote, last modified Oct. 15, 2025           #
################################################################################

##Load Libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(grid)

##Import clinic data
clinic_data <- readRDS("FULL_clinic_data.rds", refhook = NULL)


##Clean data

##Remove other surgeries
clinic_data <- clinic_data %>%
  filter(type_surgery != "Other")

#Format admission date as a date
clinic_data$date_admission <- as.Date(clinic_data$date_admission)

# Extract year-month
clinic_data <- clinic_data %>%
  mutate(
    month = floor_date(date_admission, "month"),
    year  = year(date_admission)
  ) %>%
  group_by(year, month, subdistrict) %>%
  summarise(surgery_counts = n(), .groups = "drop")



#Plot by month and year
ggplot(clinic_data, aes(x = month, y = surgery_counts, fill = subdistrict)) +
  geom_col(
    position = position_dodge2(width = 25, preserve = "single"),
    width = 20                                                   
  ) +
  labs(title = "Number of Sterilizations Conducted per Month",
       x = "Month",
       y = "Number of Surgeries") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


#Plot with year and quarters

ggplot(clinic_data, aes(x = month, y = surgery_counts, fill = subdistrict)) +
  geom_col(position = position_dodge2(width = 25, preserve = "single"), width = 20) +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "1 year",
    date_minor_breaks = "3 months"
  ) +
  labs(
    title = "Number of Sterilizations Conducted per Month",
    x = "Year / Quarter",
    y = "Number of Surgeries"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(vjust = 2),
    axis.ticks.x = element_line(color = "gray40", size = 0.6),
    axis.ticks.length.x = unit(4, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )


##Updated plot with simplified X - axis

# find range of months
range_months <- range(clinic_data$month)

# years present in data
years <- seq(as.numeric(format(range_months[1], "%Y")),
             as.numeric(format(range_months[2], "%Y")),
             by = 1)

# centered year labels (mid-year)
year_breaks <- as.Date(paste0(years, "-07-01"))

# quarter ticks (every 3 months)
quarter_breaks <- seq(from = min(clinic_data$month),
                      to   = max(clinic_data$month),
                      by   = "3 months")

# make the plot
p <- ggplot(clinic_data, aes(x = month, y = surgery_counts, fill = subdistrict)) +
  geom_col(position = position_dodge2(width = 25, preserve = "single"), width = 20) +
  scale_x_date(
    breaks = year_breaks,
    labels = date_format("%Y")
  ) +
  labs(
    title = "Number of Sterilizations Conducted per Month",
    x = "Year / Quarter",
    y = "Number of Surgeries"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_blank(),  # we'll draw custom ticks instead
    axis.text.x = element_text(vjust = 2),
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )

# draw manual ticks for years (longer + darker)
p <- p + geom_segment(
  data = data.frame(x = year_breaks),
  aes(x = x, xend = x, y = 0, yend = -max(clinic_data$surgery_counts) * 0.04),
  inherit.aes = FALSE,
  linewidth = 0.6,
  color = "black"
)

# draw manual ticks for quarters (shorter + lighter)
p <- p + geom_segment(
  data = data.frame(x = quarter_breaks),
  aes(x = x, xend = x, y = 0, yend = -max(clinic_data$surgery_counts) * 0.02),
  inherit.aes = FALSE,
  linewidth = 0.4,
  color = "gray50"
) +
  coord_cartesian(clip = "off")

p

#Plot by year
ggplot(clinic_data, aes(x = year, y = surgery_counts, fill = subdistrict)) +
  geom_col()+
  labs(title = "Number of Sterilizations Conducted per Year",
       x = "Year",
       y = "Number of Surgeries") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )+
  facet_wrap(~subdistrict)



