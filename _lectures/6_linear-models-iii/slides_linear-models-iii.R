library(tidyverse)
library(broom) 
metro_wide <- read_csv("https://clanfear.github.io/ioc_iqa/_data/metro_2021_violence_wide.csv")
head(metro_wide, 4)

dim(metro_wide) # Few rows, more columns
names(metro_wide) # A column for each month!

metro_long <- metro_wide |>
  pivot_longer(starts_with("month"), # Take each month column #<<
               names_to  = "month", # Put col names in month column #<<
               values_to = "violence") # Put values in violence column #<<
metro_long |> select(borough, month, violence) |> head(3)

dim(metro_long)

metro_long <- metro_long |>
  mutate(month = parse_number(month))
head(metro_long)

metro_wide_again <- metro_long |>
  pivot_wider(names_from   = month, # Turn months into col names #<<
              values_from  = violence, # Turn violence in col values #<<
              names_prefix = "month_") # Start col names with month_ #<<
metro_wide_again |>  head(3)

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

## rbind(glance(lm_viol), glance(lm_viol_2), glance(lm_viol_3))

list(lm_viol, lm_viol_2, lm_viol_3) |> # combine models into a list
  map(glance) |> # run glance() on each element #<<
  list_rbind() # bind results into rows

anova(lm_viol, lm_viol_2, lm_viol_3)

lm_viol |> 
  augment() |>
  ggplot(aes(x = .fitted, 
             y = .resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, 
             color = "red") +
  geom_smooth()

lm_viol_2 |> 
  augment() |>
  ggplot(aes(x = .fitted, 
             y = .resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, 
             color = "red") +
  geom_smooth()
