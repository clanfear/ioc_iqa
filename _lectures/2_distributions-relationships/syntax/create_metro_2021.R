library(tidyverse)

# LOL hilariously non-reproducible but fast

london_subregion <- bind_rows(
  data.frame(subregion = "West", borough = c("Brent", "Ealing", "Hammersmith and Fulham", "Harrow", "Richmond upon Thames", "Hillingdon", "Hounslow")),
  data.frame(subregion = "East", borough = c("Barking and Dagenham", "Bexley", "Greenwich", "Hackney", "Havering", "Lewisham", "Newham", "Redbridge", "Tower Hamlets", "Waltham Forest")),
  data.frame(subregion = "South", borough = c("Bromley", "Croydon", "Kingston upon Thames", "Merton", "Sutton", "Wandsworth")),
  data.frame(subregion = "North", borough = c( "Barnet", "Haringey", "Enfield")),
  data.frame(subregion = "Central", borough = c("Camden", "Kensington and Chelsea", "Islington", "Lambeth", "Southwark", "Westminster"))
)

borough_deprivation <- vroom::vroom("C:/Users/cl948/Downloads/indices-of-multiple-deprivation-borough.csv") %>%
  select(borough = Area, avg_deprivation = `Average Score - 2007`) %>%
  filter(borough %in% london_subregion$borough) %>%
  mutate(dep = ntile(avg_deprivation, 3)) %>%
  mutate(deprivation = case_when(
    dep ==1 ~ "Low",
    dep == 2 ~ "Medium",
    dep == 3 ~ "High",
    TRUE ~ "ERROR"
  )) %>%
  select(borough, deprivation)

borough_pop_density <- vroom::vroom("C:/Users/cl948/Downloads/housing-density-borough.csv") %>%
  filter(Year == 2021) %>%
  select(borough = Name, pop = Population, area = Square_Kilometres) %>%
  filter(borough %in% london_subregion$borough)



metro_2021 <- vroom::vroom(list.files("C:/Users/cl948/Downloads/metro_2021/", recursive = TRUE, full.names = TRUE)) %>%
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
  pivot_wider(names_from = crime_type, values_from = n) %>%
  left_join(london_subregion) %>%
  left_join(borough_deprivation)
  left_join(borough_pop_density)

write_csv(metro_2021, file = "./_data/metro_2021.csv")
