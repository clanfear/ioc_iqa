# Log outcome on homicide, no zeroes
# Quadratic disadvantage
# Interaction between control and physical opportunity
# Dis / CE -> opp
set.seed(49)
library(tidyverse)
standardize <- function(x){
  return((x - mean(x))/sd(x))
}
sample_size <- 343
neighb_crime <- 
  MASS::mvrnorm(sample_size, mu = c(0,0, 0), Sigma  = matrix(c(1, 0.3, 0.2, 
                                                            0.3, 1, 0.2, 
                                                            0.2, 0.3, 1), nrow =3)) |>
  data.frame() |>
  setNames(c("disadvantage", "instability", "heterogeneity")) |>
  mutate(across(c(disadvantage, instability, heterogeneity), ~standardize(.)),
         control = standardize(-0.3*disadvantage -0.15*instability -0.1*heterogeneity + rnorm(sample_size, 0, 1)),
         opportunity_index = ntile(0.3*disadvantage -0.2*control + rnorm(sample_size, 0, 1), 5),
         opportunity = case_when(
           opportunity_index %in% c(1, 2) ~ "low", 
           opportunity_index %in% c(3, 4) ~ "medium", 
           opportunity_index == 5 ~ "high"
           ),
         robbery = round(1 + exp(-0.25*control + 0.2*disadvantage + 0.05*instability +
           -0.05*(disadvantage^2) +
           0.5*(opportunity=="medium") + 1.25*(opportunity=="high") +
           -0.5*control*(opportunity=="low") -0.15*control*(opportunity=="medium") +
             rnorm(sample_size, 2, 0.5)), 0)
         ) |>
  select(-opportunity_index)
write_csv(neighb_crime, file = "./_data/neighb_crime.csv")

lm_true <- lm(log(robbery) ~ control*opportunity + disadvantage + I(disadvantage^2) + instability + heterogeneity, data = neighb_crime)
summary(lm_true)
lm_simple <-lm(robbery ~ control * opportunity + disadvantage + instability + heterogeneity, data = neighb_crime)
summary(lm_simple)
