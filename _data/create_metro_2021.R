library(tidyverse)

# LOL hilariously non-reproducible but fast

london_subregion <- bind_rows(
  data.frame(subregion = "West", borough = c("Brent", "Ealing", "Hammersmith and Fulham", "Harrow", "Richmond upon Thames", "Hillingdon", "Hounslow")),
  data.frame(subregion = "East", borough = c("Barking and Dagenham", "Bexley", "Greenwich", "Hackney", "Havering", "Lewisham", "Newham", "Redbridge", "Tower Hamlets", "Waltham Forest")),
  data.frame(subregion = "South", borough = c("Bromley", "Croydon", "Kingston upon Thames", "Merton", "Sutton", "Wandsworth")),
  data.frame(subregion = "North", borough = c( "Barnet", "Haringey", "Enfield")),
  data.frame(subregion = "Central", borough = c("Camden", "Kensington and Chelsea", "Islington", "Lambeth", "Southwark", "Westminster"))
)
write_csv(london_subregion, file = "./_data/london_subregion.csv")

borough_deprivation <- vroom::vroom("./_data/source_data/indices-of-multiple-deprivation-borough.csv") %>%
  select(borough = Area, avg_deprivation = `Average Score - 2007`) %>%
  filter(borough %in% london_subregion$borough) %>%
  mutate(dep = ntile(avg_deprivation, 3)) %>%
  mutate(deprivation = case_when(
    dep ==1 ~ "Low",
    dep == 2 ~ "Medium",
    dep == 3 ~ "High",
    TRUE ~ "ERROR"
  )) %>%
  select(borough, deprivation, dep_score = avg_deprivation)

write_csv(borough_deprivation, file = "./_data/borough_deprivation.csv")

borough_pop_density <- vroom::vroom("./_data/source_data/housing-density-borough.csv") %>%
  filter(Year == 2021) %>%
  select(borough = Name, pop = Population, area = Square_Kilometres) %>%
  filter(borough %in% london_subregion$borough) |>
  janitor::clean_names(case = "title")

write_csv(borough_pop_density, file = "./_data/borough_pop_density.csv")

borough_pop_density <- borough_pop_density |> janitor::clean_names()

metro_2021_crime <- vroom::vroom(list.files("C:/Users/cl948/Downloads/metro_2021/", recursive = TRUE, full.names = TRUE)) %>%
  janitor::clean_names() %>%
  filter(!is.na(lsoa_name)) %>%
  mutate(borough = str_remove(lsoa_name, " [0-9].*$")) %>% 
  group_by(borough) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1000) %>%
  filter(crime_type %in% c("Burglary", "Robbery", "Violence and sexual offences", "Anti-social behaviour")) %>%
  mutate(crime_type = str_to_lower(str_replace_all(str_remove(crime_type, "-"), " ", "_"))) %>%
  count(borough, crime_type, month) %>%
  mutate(month = lubridate::ym(month)) %>%
  pivot_wider(names_from = crime_type, values_from = n) 
 
 metro_2021_full <- metro_2021_crime %>%
  left_join(london_subregion) %>%
  left_join(borough_deprivation) %>%
  left_join(borough_pop_density)

write_csv(metro_2021_full, file = "./_data/metro_2021_full.csv")

metro_2021_crime <- metro_2021 |> select(borough, month, antisocial_behaviour, burglary, robbery, violence_and_sexual_offences)
write_csv(metro_2021_crime, file = "./_data/metro_2021_crime.csv")

metro_2021 <- metro_2021 |> select(-dep_score)
write_csv(metro_2021, file = "./_data/metro_2021.csv")

metro_2021_violence_wide <- metro_2021 |> 
  select(borough, month, violence_and_sexual_offences, deprivation, subregion, pop, area) |> 
  mutate(month = str_c("month_", lubridate::month(month))) |> 
  pivot_wider(names_from = month, values_from = violence_and_sexual_offences)
write_csv(metro_2021_violence_wide, file = "./_data/metro_2021_violence_wide.csv")
