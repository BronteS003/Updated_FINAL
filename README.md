---
editor_options: 
  markdown: 
    wrap: 72
---

# Updated_FINAL - Jai Dog Data Analysis v2
[![DOI](https://zenodo.org/badge/1036209075.svg)](https://doi.org/10.5281/zenodo.18255663)

### Archive

##### All code from previous iterations of analysis using 2021, 2023,and 2024 data

### Data Visualizations

##### All code for plotting, mapping, and general visualizations

FULL_sterilizations_overtime.R - Plotting all sterilizations over time and by subdistrict.

KAP_survey.R - Visualizing and looking at frequency of different variables in KAP survey.

clinic_rough_work.R <- Looking at misc. aspects of clinic data such as seasonality of pregnancies and
the ownership status of dogs admitted to clinic

mapping_panel.R <- Code for all map panels in final paper

sex_proportion.R <- Looking at male:female ratio of dogs admitted to clinic versus seen in survey to understand
if representative.

### Data

##### All final data files as well as RDS code and files.

FULL_clinic_data.csv - All clinic data (2021-2025) from Jai Dog's mobile and static
clinic recording all admissions including sterilizations.

clinic_data_v2.rds - Most up to date RDS file of cleaned clinic data for use in all
models.

FULL_dog_density.csv - All observations (2021-2025) from Jai Dog's sight and resight
surveys summarized by each survey conducted.

density_v2.rds - Most up to date RDS file of cleaned dog density data for use in v2 model 1 - dog density.

v2_dog_density_RDS.R - Code cleaning and formatting the
FULL_dog_density data set for use in all v2 model 1 dog density.

FULL_sightings.csv - All observations (2021-2025) from Jai Dog's sight and resight
surveys organized where each row is one dog observation.

sightings_v2.rds - RDS file of cleaned sightings data for use in all
v2 models.

v2_sightings_RDS.R - Code cleaning and formatting the FULL_sightings
data set for use in all subsequent models.

KAP.csv - Raw KAP survey responses.


### Models

##### Code for all models.

v2_day_comparison.R - Code exploring comparison of day 1 and day 2 sightings 
and if they are significant for dog count.

v2_model1_dog_density.R - Most up to date -
All code for model 1 dog density examining change in dog density against all measures of sterilization effort. 

v2_model2_sterilization_status.R - Most up to date - All code for model 2 sterilization status 
examining the probability a dog is sterilized based on all measures of sterilization effort. 

v2_model3_population_makeup.R - Most up to date - All code for model 3 looking at the
likelihood a dog is a lactating female or puppy based on all measures of sterilization effort.

v2_model4_health_status.R - Most up to date - All code for model 4 looking at the
likelihood a dog is healthy based on all measures of sterilization effort.

v3_effort_as_interaction.R - Code looking at usinf sterilization effort in models
as an interaction between effort and subdistrict.
likelihood a dog is healthy based on all measures of sterilization effort.


### Plots

##### All plots for models and data visualization.
