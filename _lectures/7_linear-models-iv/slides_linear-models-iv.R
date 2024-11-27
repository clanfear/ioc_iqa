library(tidyverse)
library(broom)
library(janitor)

metro_2021_crime    <- 
  read_csv("https://clanfear.github.io/ioc_iqa/_data/metro_2021_crime.csv")
borough_deprivation <- 
  read_csv("https://clanfear.github.io/ioc_iqa/_data/borough_deprivation.csv")
borough_pop_density <- 
  read_csv("https://clanfear.github.io/ioc_iqa/_data/borough_pop_density.csv")
london_subregion    <- 
  read_csv("https://clanfear.github.io/ioc_iqa/_data/london_subregion.csv")



names(metro_2021_crime)
names(london_subregion)
names(borough_pop_density)
names(borough_deprivation)

dim(metro_2021_crime)

dim(london_subregion)
dim(borough_pop_density)
dim(borough_deprivation)



metro_2021_crime |>
  left_join(london_subregion) |> 
  glimpse()

metro_2021_crime |>
  left_join(borough_pop_density, by = c("borough"="Borough")) |>
  glimpse()

metro_2021 <- metro_2021_crime |>
  left_join(london_subregion) |>
  left_join(borough_deprivation) |>
  left_join(borough_pop_density |> clean_names())
head(metro_2021, 3)


metro_2021 <- metro_2021 |>
  rename(violence = violence_and_sexual_offences,
         asb      = antisocial_behaviour) |>
  mutate(month    = lubridate::month(month), #<<
         pop_den = (pop/1000)/area)

0.0000000032

options(scipen = 10)
0.0000000032

lm_sq <- 
  lm(violence ~ month + I(month^2), 
   data = metro_2021) 
lm_sq |> tidy() |> 
  select(term, estimate, std.error)

ggplot(metro_2021, aes(x = month, y = violence)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~ x + I(x^2))

lm_log <- lm(violence ~ log(month), 
   data = metro_2021)
lm_log |> tidy() |> 
  select(term, estimate, std.error)

ggplot(metro_2021, aes(x = month, y = violence)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~ log(x))

exp(1:3)
curve(exp(x), from = 1, to = 3)

log(1:3)
curve(log(x), from = 1, to = 3)

lm_log |> coef()

lm(log(violence) ~ month, 
   data = metro_2021) |> coef()

lm(log(violence) ~ log(month), 
   data = metro_2021) |> coef()



ggplot(metro_2021 , 
    aes(x     = pop_den, 
        y     = violence, 
        color = deprivation)) + 
  geom_point() + 
  geom_smooth(method = "lm")

lm_int <- lm(violence ~ deprivation*pop_den, data = metro_2021)
lm_int |> tidy() |> select(term, estimate, statistic)

lm_noint <- lm(violence ~ deprivation + pop_den, data = metro_2021)
anova(lm_int, lm_noint)

lm_int |>
  augment() |>
  ggplot(
    aes(x = pop_den, 
        y = .fitted, 
        color = deprivation)) +
  geom_point() +
  geom_line()
