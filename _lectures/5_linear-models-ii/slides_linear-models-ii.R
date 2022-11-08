library(tidyverse)
library(broom) 
communities <- 
  read_csv("https://clanfear.github.io/ioc_iqa/_data/communities.csv") |>
  mutate(across(c(incarceration, disadvantage), 
                ~ factor(., levels = c("Low", "Medium", "High"))))

x <- round(runif(612, -0.49, 9.49),0)
cat(x, fill = TRUE)


s_n <- 10000
po_data <- tibble(
  bd = runif(s_n, 0, 1), # Random uniform variable
  x  = rbinom(s_n, 1, bd), # Random binary variable
  y0 = rnorm(s_n, 2*bd, 1),
  y1 = rnorm(s_n, 2*bd + 1, 1)) |> # Effect size of 1 #<<
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





## ?tidy

## ?tidy.lm # tidy() method for lm objects

## ?summary.lm # summary method for lm objects



lm(crime_rate ~ pop_density,
   data = communities) |>
  augment() |> #<<
  slice(1) |> #<<
  select(.fitted, pop_density)

ggplot(communities,
       aes(x = pop_density, 
           y = crime_rate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() + 
  geom_hline(yintercept = 35.43, color = "red") +
  geom_vline(xintercept = 18.17, color = "red")

ex_lm <- lm(crime_rate ~ pop_density + disadvantage, 
            data = communities)
fitted(ex_lm) |> head(4) # Returns them as a vector

augment(ex_lm) |> head(4) # Returns them added to original data

ex_lm |> # getting coefficients
  tidy() |>
  select(term, estimate)

communities[1, c(2, 4)]
fitted(ex_lm)[1]

predict(ex_lm, newdata = data.frame(pop_density = 10, 
                                    disadvantage = "High"))

predict(ex_lm, newdata = data.frame(pop_density = 0, 
                                    disadvantage = "Low"))

communities |> pull(pop_density) |> min()

lm_1 <- lm(crime_rate ~ disadvantage, data = communities)
glance(lm_1)

lm_2 <- lm(crime_rate ~ disadvantage + pop_density, data =communities)
lm_3 <- lm(crime_rate ~ disadvantage + pop_density + area, 
           data = communities)
rbind(glance(lm_1), # rbind() stacks output as rows #<<
      glance(lm_2), 
      glance(lm_3)) |> select(r.squared)

cor_mat <- matrix(c(1, 0.9, 0.3, 0.9, 1, 0.3, 0.3, 0.3, 1), nrow = 3)
ex_data <- MASS::mvrnorm(n = 250, rep(0, 3), cor_mat) |>
  data.frame() |> setNames(c("x", "z", "y")) # add column names
cor(ex_data) # correlation matrix

lm(y ~ x + z, data = ex_data) |> tidy()

rbind(glance(lm_1), glance(lm_2), glance(lm_3)) |> 
  select(r.squared, adj.r.squared)

anova(lm_2, lm_3)

anova(lm_1, lm_2, lm_3)

lm_4 <- lm(crime_rate ~ disadvantage + incarceration, data = communities)
anova(lm_1, lm_4)
