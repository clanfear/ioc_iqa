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
       aes(x = month, #<<
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
  labs(x = "Month",
       y = "Robbery",
       title = 
        "Robbery in Westminster") #<<

ggplot(data  = westminster, 
       aes(x = month, 
           y = robbery)) +
  geom_point(color = "red", 
             size  = 3) +
  labs(x = "Month",
       y = "Robbery",
       title = 
        "Robbery in Westminster") +
  theme_bw() #<<

ggplot(data  = westminster, 
       aes(x = month, 
           y = robbery)) +
  geom_point(color = "red", 
             size  = 3) +
  labs(x = "Month",
       y = "Robbery",
       title = 
        "Robbery in Westminster") +
  theme_bw(base_size = 18) #<<

 ggplot(data  = metro_2021, 
        aes(x = month, 
            y = robbery)) +
  geom_point(color = "red", 
             size = 3) +
  labs(x     = "Month",
       y     = "Robbery",
       title = 
         "Robbery in London") +
  theme_bw(base_size = 18)

 ggplot(data  = metro_2021, 
        aes(x = month, 
            y = robbery)) +
  geom_line(color = "red", 
            linewidth  = 3) + #<<
  labs(x     = "Month",
       y     = "Robbery",
       title = 
         "Robbery in London") +
  theme_bw(base_size = 18)

 ggplot(data  = metro_2021, 
        aes(x = month, 
            y = robbery, 
            group = borough)) +
  geom_line(color = "red", 
            linewidth  = 3) + 
  labs(x     = "Month",
       y     = "Robbery",
       title = 
         "Robbery in London")+
  theme_bw(base_size = 18)

 ggplot(data  = metro_2021, 
        aes(x = month, 
            y = robbery, 
            group = borough)) +
  geom_line(color = "red") + #<<
  labs(x     = "Month",
       y     = "Robbery",
       title = 
         "Robbery in London") +
  theme_bw(base_size = 18)

 ggplot(data  = metro_2021, 
        aes(x = month, 
            y = robbery, 
            group = borough, 
            color = subregion)) + #<<
  geom_line() + 
  labs(x     = "Month",
       y     = "Robbery",
       title = 
         "Robbery in London") +
  theme_bw(base_size = 18)

 ggplot(data  = metro_2021, 
        aes(x = month, 
            y = robbery, 
            group = borough, 
            color = subregion)) +
  geom_line() + 
  facet_wrap(~subregion) + #<<
  labs(x     = "Month",
       y     = "Robbery",
       title = 
         "Robbery in London")+
  theme_bw(base_size = 18)

 ggplot(data  = metro_2021, 
        aes(x = month, 
            y = robbery, 
            group = borough, 
            color = subregion)) +
  geom_line() + 
  facet_wrap(~subregion) +
  labs(x     = "Month",
       y     = "Robbery",
       title = 
         "Robbery in London") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") #<<

 ggplot(data  = metro_2021, 
        aes(x = month, 
            y = robbery, 
            group = borough, 
            color = subregion)) +
  geom_line() + 
  scale_x_date(date_labels = "%b") + #<<
  facet_wrap(~subregion) +
  labs(x     = "Month",
       y     = "Robbery",
       title = 
         "Robbery in London") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

## library(gganimate)
## library(ggforce)
## 
## samples <- rnorm(100)
## index <- seq(1:length(samples))
## df <- tibble(value = samples, index = index)
## 
## bin_width <- 0.25
## 
## count_data <- # some minor data transformation
##   df %>%
##   mutate(x = plyr::round_any(value, bin_width)) %>%
##   group_by(x) %>%
##   mutate(y = seq_along(x))
## 
## plot2 <-
##   ggplot(count_data) +
##   geom_ellipse(aes(group = index, x0 = x, y0 = y, a = bin_width/2, b = 0.5, angle = 0), fill = '#f0f1eb', color = NA) +
##   coord_equal(bin_width, expand = FALSE)  +# to make the dots look nice and round
##   theme_void() +
##   theme(plot.background = element_rect(fill = "transparent", color = "transparent"))
## 
## p_anim2 <-
##   plot2 +
##   transition_states(states = index, transition_length = 100, state_length = 1) +
##   shadow_mark() +
##   enter_fly(y_loc = 15)
## 
## balldrop <- animate(p_anim2, fps = 60, duration = 10, bg = 'transparent', width = 1024, height = 1024)
## anim_save("img/balldrop.gif", balldrop)

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

tibble(x = seq(0, 15, 0.1),
       density = dchisq(x, df = 2)) |> 
  ggplot(aes(x = x, y = density)) + 
  geom_line() +
  coord_cartesian(expand = FALSE) +
  geom_vline(xintercept = 9.1153, 
             color = "red")

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
  geom_smooth(method  = "lm", 
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
  geom_smooth(method  = "lm", 
              formula = "y ~ x") +
  theme_minimal(base_size = 16)
