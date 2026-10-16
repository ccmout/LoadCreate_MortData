# Benodigde pakketten installeren en laden
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("dplyr")
if (!require("onsr")) install.packages("onsr")

library(tidyverse)
library(dplyr)
library(onsr)

# exposure UK uit statistisch bureau ONS
year_ONS_lb <- 2020
year_ONS_ub <- 2024

population_data <- ons_get(id = "mid-year-pop-est")
population_filtered <- population_data %>%
  filter(`calendar-years` >= year_ONS_lb, `administrative-geography` == "K04000001") %>%
  select(`calendar-years`, Age, sex, v4_0) %>%
  rename(values = v4_0, year = `calendar-years`, age = Age) %>%
  mutate(geo = " UK", .before = values) %>%
  arrange(year, age, sex)
population_filtered$sex <- ifelse(population_filtered$sex == "all", "Total", ifelse(population_filtered$sex == "male", "Male", "Female"))
population_filtered <- pivot_wider(population_filtered, names_from = sex, values_from = values)
population_filtered <- population_filtered %>% relocate(Female, Male, Total, .after = geo)

#exposure UK voor 2023 is gemaakt!

########################################
# helaas blijkt dat de sterfte-aantallen naar leeftijdsgroepen is geaggregeerd
# daarom is de onderstaande poging op EW zinloos
# death UK uit Nomis/NRS/NISRA in 4 stappen
# 1ste stap: EW (Nomis)
# 2de stap: SC (NRS)
# 3de stap: NIRL (NISRA)
# som van stap 1, 2 en 3
if (!require("nomisr")) install.packages("nomisr")
library(nomisr)

dataset_id <- "NM_161_1"
metadata_structure <- nomis_get_metadata(id = dataset_id)
age_range <- "0"
for(i in 1:100) {age_range <- paste0(age_range, ",", i)}
mortality_data <- nomis_get_data(
  id = dataset_id,
  geography = "2092957703", # TYPE499 pulls national-level data (England and Wales)
  gender = "1,2",           # 1 = Male, 2 = Female (Excludes total aggregate)
  age = age_range,           # Pulls all available single-year and group age bands
  time = "2015",          # Pulls all available annual time periods
  measures = "20100",    # 20100 is the standard code for 'Value' (Count of deaths)
  measure = "1",
  cause_of_death = "0"
)
mortality_data_filtered <- mortality_data %>% select(DATE, GEOGRAPHY_CODE, GENDER, AGE, AGE_NAME, OBS_VALUE)
