library(tidyverse)
library(jsonlite)
library(dplyr)
library(cbsodataR)

# rm(list = ls())
dir_output <- here("output_CBS") # file.path("C:/Users/cmout/OneDrive - Goudse/Documenten/AG-AI/Prognosetafel/AG2024/Data/EU21NL22/2024-05 C43/Output_CBS")
year_CBS_lb <- 1995
year_CBS_ub <- 2024

get_odata <- function(targetUrl) {
  data <- data.frame()
  
  while(!is.null(targetUrl)){
    response <- fromJSON(url(targetUrl))
    data <- bind_rows(data,response$value)
    targetUrl <- response[["@odata.nextLink"]]
  }
  return(data)
}

# tableUrl <- "https://odata4.cbs.nl/CBS/83765NED"
# targetUrl <- paste0(tableUrl,"/Observations")
# data <- get_odata(targetUrl)
# head(data)

# tableUrl <- "https://odata4.cbs.nl/CBS/85449NED" # update verwacht 3de kwartaal 2024 (met jaar 2023); deze tabel bevat alleen de data voor tabel C, want maar 1 measure = "M000134"
# tableUrl <- "https://odata4.cbs.nl/CBS/37168"  # update verwacht 2de kwartaal 2025 (met jaar 2023 en 2024)
# tableUrl <- "https://opendata.cbs.nl/ODataFeed/OData/37168"
# tableUrl <- "https://beta-odata4.cbs.nl/CBS/37168"
# targetUrl <- paste0(tableUrl,"/Observations")
# data <- get_odata(targetUrl)
# head(data)
data <- cbs_get_data("37168")
data <- cbs_add_label_columns(data)
df_death <- 
  data %>% select(Geslacht_label, Leeftijd, BurgerlijkeStaat_label, Perioden_label, OverledenenLeeftijdBijOverlijden_1) %>% 
    filter(BurgerlijkeStaat_label == "Totaal burgerlijke staat", Leeftijd %in% c(10010:19905, 22300), Perioden_label %in% c(year_CBS_lb:year_CBS_ub))
df_death$Geslacht_label <- ifelse(df_death$Geslacht_label == "Mannen", "Male", ifelse(df_death$Geslacht_label == "Vrouwen", "Female", "Total"))
df_death$Leeftijd <- ifelse(df_death$Leeftijd == 22300, 105, 
                            ifelse(df_death$Leeftijd == 10010, 0,
                                   (as.integer(df_death$Leeftijd) - 10000) %/% 100 + as.integer(df_death$Leeftijd) %% 100))
df_death$BurgerlijkeStaat_label <- NULL
# df_death <- data %>% select(-c(1, 3, 5)) %>% filter(BurgerlijkeStaat == "T001019")
# df_death <- df_death %>% filter(Leeftijd %in% c(10010:19905, 22300))
# df_death <- df_death %>% filter(Perioden %in% paste0(c(year_CBS_lb:year_CBS_ub), "JJ00"))
# df_death$BurgerlijkeStaat <- NULL
# df_death$Geslacht <- ifelse(df_death$Geslacht == "3000", "Male", ifelse(df_death$Geslacht == "4000", "Female", "Total"))
# df_death$Leeftijd <- ifelse(df_death$Leeftijd == 22300, 105, 
#                             ifelse(df_death$Leeftijd == 10010, 0,
#                             (as.integer(df_death$Leeftijd) - 10000) %/% 100 + as.integer(df_death$Leeftijd) %% 100))
# df_death$Perioden <- as.integer(substring(df_death$Perioden, 1, 4))

df_death <- pivot_wider(df_death, names_from = Geslacht_label, values_from = OverledenenLeeftijdBijOverlijden_1)
df_death$Age <- df_death$Leeftijd
df_death$Leeftijd <- NULL
df_death$Year <- df_death$Perioden_label
df_death$Perioden_label <- NULL
df_death <- relocate(df_death, Year, Age, Female, Male, .before = Total)
df_death <- df_death %>% arrange(Year, Age)
# df_death <- pivot_wider(df_death, names_from = Geslacht, values_from = Value)

# df_death_C <- df_death %>% filter(Measure == "M003614") %>% select(-c("Measure"))
# df_death_C <- df_death_C %>% arrange(Year, Age)
df_death_C <- df_death %>% 
  mutate(F_exposure = (Female - lead(Female, 1))/2, 
         M_exposure = (Male - lead(Male, 1))/2, 
         T_exposure = (Total - lead(Total, 1))/2)
df_death_C$F_exposure[df_death_C$Age == 0] <- df_death_C$Female[df_death_C$Age == 0] - df_death_C$Female[df_death_C$Age == 1]/2
df_death_C$M_exposure[df_death_C$Age == 0] <- df_death_C$Male[df_death_C$Age == 0] - df_death_C$Male[df_death_C$Age == 1]/2
df_death_C$T_exposure[df_death_C$Age == 0] <- df_death_C$Total[df_death_C$Age == 0] - df_death_C$Total[df_death_C$Age == 1]/2

df_death_D <- df_death
# df_death_D <- df_death %>% filter(Measure == "M003613") %>% select(-c("Measure"))
rm(df_death)

data <- cbs_get_data("7461bev")
data <- cbs_add_label_columns(data)
# tableUrl <- "https://odata4.cbs.nl/CBS/7461bev"
# targetUrl <- paste0(tableUrl,"/Observations") # update verwacht 2de kwartaal 2024 (met stand per 1 jan 2024)
# data <- get_odata(targetUrl)
# head(data)

df_exp <- data %>% select(-c(1, 2, 3, 5)) %>% filter(BurgerlijkeStaat == "T001019")
df_exp <- df_exp %>% filter(Leeftijd %in% c(10010:19905, 22300))
df_exp <- df_exp %>% filter(Perioden %in% paste0(c(year_CBS_lb:year_CBS_ub), "JJ00"))
df_exp$Measure <- NULL
df_exp$BurgerlijkeStaat <- NULL
df_exp$Geslacht <- ifelse(df_exp$Geslacht == "3000", "Male", ifelse(df_exp$Geslacht == "4000", "Female", "Total"))
df_exp$Leeftijd <- ifelse(df_exp$Leeftijd == 22300, 105, 
                          ifelse(df_exp$Leeftijd == 10010, 0,
                          (as.integer(df_exp$Leeftijd) - 10000) %/% 100 + as.integer(df_exp$Leeftijd) %% 100))
df_exp$Perioden <- as.integer(substring(df_exp$Perioden, 1, 4))

df_exp$Age <- df_exp$Leeftijd
df_exp$Leeftijd <- NULL
df_exp$Year <- df_exp$Perioden
df_exp$Perioden <- NULL
df_exp <- pivot_wider(df_exp, names_from = Geslacht, values_from = Value)
df_exp <- df_exp %>% arrange(Age, Year)
df_exp <- df_exp %>%
  mutate(F_exposure = (Female + lead(Female, 1))/2, 
         M_exposure = (Male + lead(Male, 1))/2, 
         T_exposure = (Total + lead(Total, 1))/2)

df_exp <- df_exp %>% inner_join(df_death_C, by = c("Year", "Age"), suffix = c("_P", "_C"))
df_exp$F_exposure <- df_exp$F_exposure_P + df_exp$F_exposure_C/6
df_exp$M_exposure <- df_exp$M_exposure_P + df_exp$M_exposure_C/6
df_exp$T_exposure <- df_exp$T_exposure_P + df_exp$T_exposure_C/6
df_exp <- df_exp[!(is.na(df_exp$T_exposure)),]
df_exp$Female_P <- NULL
df_exp$Male_P <- NULL
df_exp$Total_P <- NULL
df_exp$Female_C <- NULL
df_exp$Male_C <- NULL
df_exp$Total_C <- NULL
df_exp$F_exposure_P <- NULL
df_exp$M_exposure_P <- NULL
df_exp$T_exposure_P <- NULL
df_exp$F_exposure_C <- NULL
df_exp$M_exposure_C <- NULL
df_exp$T_exposure_C <- NULL
colnames(df_exp)[3:5] <- c("Female", "Male", "Total")
df_exp$Female <- format(round(df_exp$Female, 2), nsmall = 2)
df_exp$Male <- format(round(df_exp$Male, 2), nsmall = 2)
df_exp$Total <- format(round(df_exp$Total, 2), nsmall = 2)
df_exp <- df_exp %>% arrange(Year, Age)
df_exp <- relocate(df_exp, Year, .before = Age)

filename_out <- file.path(dir_output, paste0("Deaths", "_"  , "NL", "_CBS", ".txt"))
write_delim(df_death_D, filename_out, delim = "\t")
filename_out <- file.path(dir_output, paste0("Exposures", "_"  , "NL", "_CBS", ".txt"))
write_delim(df_exp, filename_out, delim = "\t")




