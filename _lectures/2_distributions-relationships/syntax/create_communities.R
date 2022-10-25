library(tidyverse)
communities <- tibble(Area = rep(c("Rural", "Urban"), each = 5000),
                `Pop Density` = c(rnorm(5000, 10, 2),
                                  rnorm(5000, 20, 3)),
                `Crime Rate` = (rpois(10000, `Pop Density`)^2) * round(rnorm(10, 10000, 500)) / 100000) |>
  mutate(Disadvantage = 0.3*scale(`Crime Rate`) - 0.1*scale(`Pop Density`) + rnorm(5000)) |>
  mutate(Incarceration = 0.3*scale(`Crime Rate`) + 0.2*scale(`Pop Density`) + 0.2*scale(Disadvantage) + rnorm(5000)) |>
  mutate(Disadvantage = factor(ntile(Disadvantage, 3), labels = c("Low", "Medium", "High")),
         Incarceration = factor(ntile(Incarceration, 3), labels = c("Low", "Medium", "High"))) |>
  sample_n(300) |>
  janitor::clean_names()

write_csv(communities, file = "_data/communities.csv")


