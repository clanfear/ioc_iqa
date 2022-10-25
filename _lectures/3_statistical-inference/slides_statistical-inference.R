library(tidyverse)
metro_2021 <- 
  read_csv("https://clanfear.github.io/ioc_iqa/_data/metro_2021.csv")

glimpse(metro_2021)

metro_2021 |> 
  distinct(borough) |>
  head()

metro_2021 |> 
  summarize(n_boroughs  = n_distinct(borough),
            n_months    = n_distinct(month),
            sample_size = n())

32 * 12

metro_2021 |> count(borough) |> head(3)

metro_2021 |> count(borough) |> count(n) # This makes a warning! #<< 

westminster <- metro_2021 |> filter(borough == "Westminster")
head(westminster, 4)

plot(robbery ~ month, 
     data = westminster, 
     xlab = "Month", 
     ylab = "Robbery",
     main = 
       "Robbery in Westminster", 
     col = "red", 
     cex.lab = 1.5,
     cex.main= 1.5,
     pch = 16)

library(ggplot2)

ggplot(data = westminster, 
        aes(x = month, 
            y = robbery)) +
  geom_point()

ggplot(data  = westminster,  #<<
       aes(x = month, 
           y = robbery)) #<<

ggplot(data  = westminster, 
       aes(x = month, 
           y = robbery)) +
    geom_point() #<<

ggplot(data  = westminster, 
       aes(x = month, 
           y = robbery)) +
  geom_point(color = "red", 
             size  = 3) #<<

ggplot(data  = westminster, 
       aes(x = month, 
           y = robbery)) +
  geom_point(color = "red", 
             size  = 3) +
  labs(x = "Month") #<<

ggplot(data  = westminster, 
       aes(x = month, 
           y = robbery)) +
  geom_point(color = "red", 
             size  = 3) +
  labs(x = "Month",
       y = "Robbery") #<<

ggplot(data  = westminster, 
       aes(x = month, 
           y = robbery)) +
  geom_point(color = "red",
             size  = 3) +
  labs(
    x = "Month",
    y = "Robbery",
    title = 
      "Robbery in Westminster") #<<

ggplot(data  = westminster, 
       aes(x = month, 
           y = robbery)) +
  geom_point(color = "red", 
             size  = 3) +
  labs(
    x = "Month",
    y = "Robbery",
    title = 
      "Robbery in Westminster") +
  theme_bw() #<<

ggplot(data  = westminster, 
       aes(x = month, 
           y = robbery)) +
geom_point(color = "red", 
           size  = 3) +
  labs(
    x = "Month",
    y = "Robbery",
    title = 
      "Robbery in Westminster") +
   theme_bw(base_size = 18) #<<

 ggplot(data  = metro_2021, 
        aes(x = month, 
            y = robbery)) +
  geom_point(color = "red", 
             size = 3) +
  labs(
    x = "Month",
    y = "Robbery",
    title = 
      "Robbery in London") +
   theme_bw(base_size = 18) #<<

 ggplot(data = metro_2021, 
      aes(x = month, 
          y = robbery)) +
  geom_line(color = "red", 
            size = 3) + #<<
  labs(
    x = "Month",
    y = "Robbery",
    title = "Robbery in London") +
  theme_bw(base_size = 18)

 ggplot(data = metro_2021, 
      aes(x = month, 
          y = robbery, 
          group = borough)) +
  geom_line(color = "red", 
            size = 3) + #<<
  labs(
    x = "Month",
    y = "Robbery",
    title = "Robbery in London") +
  theme_bw(base_size = 18)

 ggplot(data = metro_2021, 
      aes(x = month, 
          y = robbery, 
          group = borough)) +
  geom_line(color = "red") + #<<
  labs(
    x = "Month",
    y = "Robbery",
    title = "Robbery in London") +
  theme_bw(base_size = 18)

 ggplot(data = metro_2021, 
      aes(x = month, 
          y = robbery, 
          group = borough,
          color = subregion)) + #<<
  geom_line() + 
  labs(
    x = "Month",
    y = "Robbery",
    title = "Robbery in London") +
  theme_bw(base_size = 18)

 ggplot(data = metro_2021, 
      aes(x = month, 
          y = robbery, 
          group = borough,
          color = subregion)) + #<<
  geom_line() + 
  facet_wrap(~subregion) +
  labs(
    x = "Month",
    y = "Robbery",
    title = "Robbery in London") +
  theme_bw(base_size = 18)

 ggplot(data = metro_2021, 
      aes(x = month, 
          y = robbery,
          group = borough,
          color = subregion)) + #<<
  geom_line() + 
  facet_wrap(~subregion) +
  labs(
    x = "Month",
    y = "Robbery",
    title = "Robbery in London") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

communities <- 
  read_csv("https://clanfear.github.io/ioc_iqa/_data/communities.csv") |> 
  mutate(disadvantage = 
           factor(disadvantage, levels = c("Low", "Medium", "High")), 
         incarceration = 
           factor(incarceration, levels = c("Low", "Medium", "High")))
glimpse(communities)

library(janitor) # For the tabyl function
communities |> tabyl(disadvantage, area) |> adorn_title()

communities |> tabyl(disadvantage, area) |> chisq.test()

1 - pchisq(9.1153, df = 2)

communities |> 
  tabyl(disadvantage, area) |> 
  adorn_title() |> 
  knitr::kable() # Access a package function without loading it #<<

library(flextable)
communities |> 
  rename(Disadvantage = disadvantage) |>
  tabyl(Disadvantage, area) |>
  flextable() |>
  add_header_lines("Table 1. Disadvantage by area")

ggplot(communities, 
       aes(x = disadvantage)) + 
  geom_bar()

ggplot(communities, 
       aes(x    = disadvantage,
           fill = area)) + 
  geom_bar()

ggplot(communities, 
       aes(x    = disadvantage,
           fill = area)) + 
  geom_bar(position = "dodge")

cr_pd_lm <- lm(crime_rate ~ pop_density, data = communities)
cr_pd_lm

summary(cr_pd_lm)

str(cr_pd_lm, list.len=6) # Truncating the output a bit

cr_pd_lm$coefficients

summary(cr_pd_lm)$coefficients

library(broom)
tidy(cr_pd_lm)

tidy(cr_pd_lm) %>% filter(term == "pop_density")

cr_pd_lm |> augment() |> glimpse()

communities %>%
  ggplot(aes(x = pop_density, 
             y = crime_rate)) + 
  geom_point(alpha = 0.5) +
  labs(x = "Population Density", 
       y = "Crime Rate") +
  geom_smooth(method = "lm", 
              formula = "y ~ x") +
  theme_minimal(base_size = 16)

communities %>%
  ggplot(aes(x = pop_density, 
             y = crime_rate,
             group = area,
             color = area)) + 
  geom_point(alpha = 0.5) +
  labs(x = "Population Density", 
       y = "Crime Rate",
       color = "Area") +
  geom_smooth(method = "lm", 
              formula = "y ~ x") +
  theme_minimal(base_size = 16)
