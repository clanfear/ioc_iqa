library(tidyverse)

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
  mutate(month = as.numeric(str_extract(month, "[0-9]+$"))) %>%
  pivot_wider(names_from = crime_type, values_from = n)

write_csv(metro_2021, file = "./_data/metro_2021.csv")
