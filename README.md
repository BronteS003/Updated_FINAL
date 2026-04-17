---
editor_options: 
  markdown: 
    wrap: 72
---

# Updated_FINAL - Jai Dog Data Analysis
[![DOI](https://zenodo.org/badge/1036209075.svg)](https://doi.org/10.5281/zenodo.18255663)

**Archive**

All code from previous iterations of analysis using 2021, 2023,and 2024
data

**Data & RDS**

All final data files as well as RDS code and files.

FULL_clinic_data.csv - All clinic data from Jai Dog's mobile and static
clinic recording all admissions including sterilizations.

FULL_clinic_data.rds - RDS file of cleaned clinic data for use in all
models.

FULL_dog_density.csv - All observations from Jai Dog's sight and resight
surveys summarized by each survey conducted.

FULL_dog_density.rds - RDS file of cleaned dog density data for use in
all models.

FULL_dog_density_RDS.R - Code cleaning and formatting the
FULL_dog_density data set for use in all subsequent models.

FULL_sightings.csv - All observations from Jai Dog's sight and resight
surveys organized where each row is one dog observation.

FULL_sightings.rds - RDS file of cleaned sightings data for use in all
models.

FULL_sightings_RDS.R - Code cleaning and formatting the FULL_sightings
data set for use in all subsequent models.

**Data Visualizations**

Any code used to create visualizations of data, separate from models or
predictions.

FULL_sterilizations_overtime.R - Code for plotting the rate of
sterilizations in each subdistrict overtime (by month and by year).

**Models**

Code for all models.

FULL_model1_dog_density.R - All code for model 1 looking at the effect
on dog density.

FULL_model2_sterilization_status.R - All code for model 2 looking at the
change in the likelihood a dog is sterilized.

FULL_model3_population_makeup.R - All code for model 3 looking at the
likelihood a dog is a lactating female or puppy.

FULL_model4_health_status.R - All code for model 4 looking at the
likelihood a dog is healthy.

day_comparison.R - All code looking at whether the day of surveying has
a significant impact on dog count and if the two days of surveying are
statistically different.

**Plots**

All plots for models and data visualization.

FULL_m1_plots.jpeg - Plots showing the probability of observing a dog
per km over total sterilization effort and time since the peak of the
intervention.

FULL_m2_probability_sex.jpeg - Plot showing the probability that a dog
is sterilized by sex.

FULL_m2_sterilization_effort_plots.jpeg - Plot showing the probability
of a dog being neutered by sterilization effort in past years.

FULL_sterilizations_by_month.jpeg - Plot showing the number of
sterilizations conducted each month from Spring 2022 to Autumn 2025 for
each subdistrict.

FULL_total_annual_sterilizations.jpeg - Plot showing the number of
sterilizations each year (2022-2025) by subdistrict.
