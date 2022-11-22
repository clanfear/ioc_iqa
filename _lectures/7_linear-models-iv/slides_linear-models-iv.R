library(tidyverse)
library(broom)
metro_2021 <- read_csv(
  "https://clanfear.github.io/ioc_iqa/_data/metro_2021_full.csv") |>
  rename(violence = violence_and_sexual_offences,
         asb      = antisocial_behaviour) |>
  mutate(month    = lubridate::month(month),
         pop_den = (pop/area)/1000)

lm_sq <- lm(violence ~ month + I(month^2), 
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
