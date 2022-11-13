library(tidyverse)
library(broom) 
communities <- 
  read_csv("https://clanfear.github.io/ioc_iqa/_data/communities.csv") |>
  mutate(across(c(incarceration, disadvantage), 
                ~ factor(., levels = c("Low", "Medium", "High"))))

lm_1 <- lm(crime_rate ~ disadvantage, data = communities)
lm_2 <- lm(crime_rate ~ disadvantage + pop_density, data =communities)
lm_3 <- lm(crime_rate ~ disadvantage + pop_density + area, 
           data = communities)

anova(lm_2, lm_3)

anova(lm_1, lm_2, lm_3)

lm_4 <- lm(crime_rate ~ disadvantage + incarceration, data = communities)
anova(lm_1, lm_4)

metro_wide <- read_csv("https://clanfear.github.io/ioc_iqa/_data/metro_2021_violence_wide.csv")
head(metro_wide)

dim(metro_wide)
names(metro_wide)

metro_long <- metro_wide |>
  pivot_longer(starts_with("month"), # Take each month column #<<
               names_to  = "month", # Put col names in month column #<<
               values_to = "violence") # Put values in violence column #<<
metro_long |> select(borough, month, violence) |> head(3)

dim(metro_long)

metro_long <- metro_long |>
  mutate(month = parse_number(month))
head(metro_long)

lm_viol <- lm(violence ~ month, data = metro_long)
lm_viol |> tidy() |> select(term, estimate, std.error)

lm_viol |> 
  augment() |>
  ggplot(aes(x = month, 
             y = .resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, 
             color = "red")

lm_viol |> 
  augment() |>
  ggplot(aes(x = month, 
             y = .resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, 
             color = "red") +
  geom_smooth() #<<

curve(2 + 0.5*x + 0.25*x^2, from = -3, to = 1, ylab = "y")

lm_viol_2 <- lm(violence ~ month + I(month^2), data = metro_long)
lm_viol_2 |> tidy() |> select(term, estimate, std.error, p.value)

metro_long |> 
  ggplot(aes(x = month, 
             y = violence)) + 
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x^2)) #<<

lm_viol_2 |> 
  augment() |>
  ggplot(aes(x = month, 
             y = .resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, 
             color = "red") +
  geom_smooth() #<<

lm_viol_2 |> tidy() |> select(term, estimate, std.error, p.value)

lm_viol_3 <- lm(violence ~ month + I(month^2) + I(month^3), 
                data = metro_long)
rbind(glance(lm_viol), glance(lm_viol_2), glance(lm_viol_3)) |> 
  select(r.squared, adj.r.squared)

anova(lm_viol, lm_viol_2, lm_viol_3)
