library(tidyverse)
library(broom) # Going to use this a bunch today
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
  tidy() |> 
  select(term, estimate)


communities |>
  group_by(incarceration) |>
  summarize(crime_rate = 
              mean(crime_rate))

aov(crime_rate ~ incarceration, data = communities) |> summary()

aov(crime_rate ~ incarceration, data = communities) |> coef()

summary(lm(crime_rate ~ incarceration, data = communities))

lm(crime_rate ~ pop_density + area,
   data = communities) |> 
  tidy() |>
  select(term, estimate)

residualized_data <- communities |>
  group_by(area) |>
  mutate(pop_density_res =  #<<
      pop_density - mean(pop_density), #<<
         crime_rate_res  =  #<<
      crime_rate - mean(crime_rate)) #<<
lm(crime_rate_res ~ pop_density_res, #<<
   data = residualized_data)  |> 
  tidy() |> select(term, estimate)

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

lm_res <- lm(crime_rate ~ pop_density, data = communities) |> augment()

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
  tidy() |> select(term, estimate)

lm(crime_rate_res ~ area_res, #<<
   data = residualized_data) |> #<<
  tidy() |> select(term, estimate)

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

sample_size <- 10000
sim_data <- 
  tibble(bd =
    rnorm(sample_size, #<<
          mean = 3, sd = 1)) #<<
sim_data |>
  summarize(bd_mean = mean(bd), 
            bd_sd   = sd(bd))

ggplot(sim_data, 
       aes(x = bd)) +
  geom_density()

sim_data <- sim_data |>
  mutate(
    x = rnorm(n(), 0.3*bd, 1),
    y = rnorm(n(), 3 + 0.3*x + 0.3*bd, 1), #<<
    coll = rnorm(n(), -0.2*x + -0.2*y, 1))



lm(y ~ x + bd, #<<
   data = sim_data) |>
  tidy() |> 
  select(term, estimate)

lm(y ~ x , #<<
   data = sim_data) |>
  tidy() |> 
  select(term, estimate)

lm(y ~ x + bd, #<<
   data = sim_data) |>
  tidy() |> 
  select(term, estimate)

lm(y ~ x + bd + coll, #<<
   data = sim_data) |>
  tidy() |> 
  select(term, estimate)

coll_data <- tibble(Skill     = runif(1000, 0, 10), 
                  Frequency = runif(1000, 0, 10), 
                  Who =   c(rep("Arrested", 500), 
                            rep("Everyone", 500))) |>
  filter(Who == "Everyone" | (10 + (-1*Skill) + Frequency > 10))


lm(Frequency ~ Skill, #<<
   data = coll_data |> 
     filter(Who=="Everyone")) |>
  tidy() |> 
  select(term, estimate)

lm(Frequency ~ Skill, #<<
   data = coll_data |> 
     filter(Who=="Arrested")) |>
  tidy() |> 
  select(term, estimate)

ggplot(coll_data, aes(x = Skill, y = Frequency, color = Who)) + 
  facet_wrap(~Who) + geom_point() +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Skill at Crime", y = "Frequency of Crime") +
  theme_minimal(base_size = 16) + theme(legend.position = "none")

sample_size <- 10000
po_data <- tibble(
  bd = runif(sample_size, 0, 1), # Random uniform variable
  x  = rbinom(sample_size, 1, bd), # Random binary variable
  y0 = rnorm(sample_size, 2*bd, 1),
  y1 = rnorm(sample_size, 2*bd + 1, 1)) |> # Effect size of 1 #<<
  mutate(y = ifelse(x==1, y1, y0)) # Treatment just selects outcome

lm(y ~ x + bd, data = po_data) |>
  tidy() |> 
  select(term, estimate)

lm(y ~ x, data = po_data) |>
  tidy() |> 
  select(term, estimate)

sample(0:1, 20, replace= TRUE)

po_data <- po_data |>
  mutate(treat = sample(0:1, n(), replace=TRUE),
         yt = ifelse(treat==1, y1, y0))

lm(y ~ x + bd, data = po_data) |>
  tidy() |> 
  select(term, estimate)

lm(yt ~ treat, data = po_data) |>
  tidy() |> 
  select(term, estimate)
