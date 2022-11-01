library(tidyverse)
communities <- 
  read_csv("https://clanfear.github.io/ioc_iqa/_data/communities.csv") |>
  mutate(across(c(incarceration, disadvantage), 
                ~ factor(., levels = c("Low", "Medium", "High"))),
         area = factor(area, levels = c("Rural", "Urban")))

## mutate(incarceration =
##          factor(incarceration, levels = c("Low", "Medium", "High")),
##        disadvantage =
##          factor(disadvantage, levels = c("Low", "Medium", "High")))

communities |>
  group_by(incarceration) |>
  summarize(across(c(crime_rate, pop_density), 
                   list(mu = ~mean(.), 
                        sd = ~sd(.))))

lm(crime_rate ~ pop_density, 
   data = communities) |> 
  coef() # extract coefficients

ggplot(communities, 
       aes(x = pop_density, 
           y = crime_rate)) + 
  geom_point() + 
  geom_smooth(method = "lm")

lm(crime_rate ~ area, 
   data = communities) |>
  coef()

ggplot(communities, 
       aes(x = area, 
           y = crime_rate)) + 
  geom_point() + 
  geom_smooth(method = "lm")

lm(crime_rate ~ area, 
   data = communities) |>
  coef()

communities |> 
  mutate(urban = 
    as.numeric(area=="Urban")) |>
  ggplot(aes(x = urban, 
             y = crime_rate)) + 
  geom_point() + 
  geom_smooth(method = "lm")

dummy_model <- 
  lm(crime_rate ~ area, 
     data = communities)
dummy_model |> coef()

communities |>
  group_by(area) |>
  summarize(crime_rate = 
              mean(crime_rate))

summary(dummy_model)$coefficients

t.test(crime_rate ~ area, data = communities, var.equal=TRUE)

lm(crime_rate ~ incarceration, 
   data = communities) |> 
  broom::tidy() |> 
  select(term, estimate)


communities |>
  group_by(incarceration) |>
  summarize(crime_rate = 
              mean(crime_rate))

aov(crime_rate ~ incarceration, data = communities) |> summary()

aov(crime_rate ~ incarceration, data = communities) |> coef()

summary(crime_rate ~ incarceration, data = communities)

lm(crime_rate ~ pop_density + area,
   data = communities) |> 
  broom::tidy() |>
  select(term, estimate)

residualized_data <- communities |>
  group_by(area) |>
  mutate(pop_density_res =  #<<
      pop_density - mean(pop_density), #<<
         crime_rate_res  =  #<<
      crime_rate - mean(crime_rate)) #<<
lm(crime_rate_res ~ pop_density_res, #<<
   data = residualized_data)  |> 
  broom::tidy() |> select(term, estimate)

lm_res <- lm(crime_rate ~ pop_density, data = communities) |> broom::augment()

ggplot(lm_res, 
    aes(x = pop_density, 
        y = crime_rate)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(lm_res,
    aes(x = pop_density, 
        y = .resid)) +  #<<
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(communities, 
       aes(x = pop_density, 
           y = crime_rate, 
           color = area)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              color = "black") 

ggplot(residualized_data, 
       aes(x = pop_density_res, #<<
           y = crime_rate_res, #<<
           color = area))+ 
  geom_point() + 
  geom_smooth(method = "lm",
              color = "black")

ggplot(residualized_data, 
       aes(x = pop_density_res,
           y = crime_rate_res))+ 
  geom_point() + 
  geom_smooth(method = "lm")

lm(crime_rate ~ pop_density + 
   area, 
   data = communities) |> 
  car::avPlot(
    variable = "pop_density")

residualized_data <- communities %>%
  mutate(area_num       = as.numeric(area == "Urban"),
         crime_rate_res = residuals(lm(crime_rate ~ pop_density, data = .)), #<<
         area_res       = residuals(lm(area_num ~ pop_density, data = .))) #<<

lm(crime_rate ~ pop_density + area, 
   data = communities) |> 
  broom::tidy() |> select(term, estimate)

lm(crime_rate_res ~ area_res, #<<
   data = residualized_data) |> #<<
  broom::tidy() |> select(term, estimate)

ggplot(residualized_data, 
       aes(x = area_num, 
           y = crime_rate, 
           color = area)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              color = "black") 

ggplot(residualized_data, 
       aes(x = area_res, #<<
           y = crime_rate_res, #<<
           color = area))+ 
  geom_point() + 
  geom_smooth(method = "lm",
              color = "black")

ggplot(residualized_data, 
       aes(x = area_res, #<<
           y = crime_rate_res, #<<
           color = area))+ 
  geom_point() + 
  geom_smooth(method = "lm",
              color = "black")

lm(crime_rate ~ pop_density + 
       area, 
   data = communities) |> 
    car::avPlot(
        variable = "areaUrban")

ex_data <- tibble(Skill = runif(1000, 0, 10), 
                  Frequency = runif(1000, 0, 10), 
                  Who = c(rep("Arrested", 500), 
                          rep("Everyone", 500))) |>
  filter(Who == "Everyone" | (10 + (-1*Skill) + Frequency > 10))

ex_data |> 
  ggplot(aes(x = Skill, y = Frequency, color = Who)) + 
  geom_point() + 
  labs(x = "Skill at Crime", y = "Frequency of Crime") +
  geom_smooth(method = "lm", formula = "y~x", se = FALSE, color = "black") + 
  facet_wrap(~Who)  + 
  theme_minimal(base_size = 16) + 
  theme(legend.position = "none",
        panel.spacing.x = unit(0.4, "in"))
