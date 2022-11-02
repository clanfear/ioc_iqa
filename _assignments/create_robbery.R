library(tidyverse)


sample_size <- 300
street_crime <- tibble(
  sex = sample(c("Male", "Female"), sample_size, replace=TRUE),
  sex_num = as.numeric(sex=="Male"),
  age = round(rnorm(sample_size, 21, 3)),
  pr_rob = 0.1 + 0.2*sex_num + 0.005*age,
  pr_theft = 0.5 - 0.1*sex_num - 0.005*age,
  pr_assault = 0.2 + 0.3*sex_num,
  ) |>
  rowwise() |>
  mutate(crime_type = sample(c("Robbery", "Theft", "Assault"), 
                             size = 1, 
                             prob = c_across(c(pr_rob, pr_theft, pr_assault)), 
                             replace=TRUE)) |>
  ungroup() |>
  mutate(mean_months = 
           4 +
           12 * as.numeric(crime_type=="Robbery") +
           3  * as.numeric(crime_type=="Assault") + 
           2 * sex_num + 
           (age/4),
         sentence = rnbinom(n(), mean_months, 0.7))


lm(sentence ~ sex + age + crime_type, data = street_crime) |> summary()

ggplot(street_crime, aes(x = age, y = sentence)) + geom_point() + geom_smooth(method = "lm")

street_crime |> janitor::tabyl(sex, crime_type)

sample(c("Robbery", "Theft", "Assault"), size = sample_size, prob = c(0.1 + 0.2*sex_num, 
                                                                      0.5 - 0.1*sex_num, 
                                                                      0.2 + 0.3*sex_num ), replace=TRUE)

sample(c("Robbery", "Theft", "Assault"), size = sample_size, replace= TRUE, prob = c(0.1, 0.5, 0.4))
rpois(10, 2)


