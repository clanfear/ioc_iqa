USArrests[c("California", "Arkansas"), 2:3]

USArrests[USArrests$Murder > 15, ]

USArrests$Murder > 15

c(1,2,3,4)[c(TRUE, FALSE, TRUE, FALSE)]

USArrests[USArrests$Murder > 15 & USArrests$Assault > 300, ]

USArrests[USArrests$Murder > 15 | USArrests$Assault > 300, ]

vector_w_missing <- c(1, 2, NA, 4, 5, 6, NA)

mean(vector_w_missing)

mean(vector_w_missing, na.rm=TRUE)

vector_w_missing == NA

is.na(vector_w_missing)

mean(vector_w_missing[!is.na(vector_w_missing)]) #<<

library(readr)
library(ggplot2)
library(dplyr)

communities <- 
  read_csv(
    "https://clanfear.github.io/ioc_iqa/_data/communities.csv"
    )

glimpse(communities)

proportions(table(communities$disadvantage))

communities |> pull(disadvantage) |> table() |> proportions()

communities |>
  mutate(high_crime = crime_rate > mean(crime_rate)) |>
  head(4)

communities <- communities |> #<<
  mutate(disadvantage = 
           factor(disadvantage, levels = c("Low", "Medium", "High")), #<<
         incarceration = 
           factor(incarceration, levels = c("Low", "Medium", "High"))
         )

communities |> pull(disadvantage) |> table()

communities |> pull(incarceration) |> table()

communities |> filter(incarceration == "High") |> head()

## communities |>
##   filter(disadvantage == "Low" &
##          crime_rate > 40)

## communities |>
##   filter(disadvantage == "Low" |
##          crime_rate > 40)

communities |> count(incarceration)

communities |> count(incarceration) |> mutate(proportion = n/sum(n))

## install.packages("janitor")

library(janitor)

communities |> tabyl(incarceration)

communities |>
  summarize(mean_crime_rate = mean(crime_rate),
            sd_crime_rate = sd(crime_rate), 
            n = n())

communities |> tabyl(disadvantage, incarceration)

communities |> tabyl(disadvantage, incarceration) |>
  adorn_percentages() # converts to cell percentages #<<

communities |> 
  tabyl(disadvantage, incarceration) # make table

communities |> 
  tabyl(disadvantage, incarceration) |> # make table
  adorn_totals(c("row", "col")) # add row/col totals

communities |> 
  tabyl(disadvantage, incarceration) |> # make table
  adorn_totals(c("row", "col")) |> # add row/col totals
  adorn_percentages()# make cells percentages

communities |> 
  tabyl(disadvantage, incarceration) |> # make table
  adorn_totals(c("row", "col")) |> # add row/col totals
  adorn_percentages() |> # make cells percentages
  adorn_pct_formatting(digits = 2) # round to 2 digits

communities |> 
  tabyl(disadvantage, incarceration) |> # make table
  adorn_totals(c("row", "col")) |> # add row/col totals
  adorn_percentages() |> # make cells percentages
  adorn_pct_formatting(digits = 2) |> # round to 2 digits
  adorn_ns() # add counts in parentheses

communities |> 
  tabyl(disadvantage, incarceration) |> # make table
  adorn_totals(c("row", "col")) |> # add row/col totals
  adorn_percentages() |> # make cells percentages
  adorn_pct_formatting(digits = 2) |> # round to 2 digits
  adorn_ns() |> # add counts in parentheses
  adorn_title() # add col variable name

communities |>
  filter(disadvantage=="High") |> #<<
  summarize(
   mean_crime = mean(crime_rate),
   sd_crime   = sd(crime_rate), 
   n          = n())

communities |>
  filter(disadvantage=="Low") |> #<<
  summarize(
   mean_crime = mean(crime_rate),
   sd_crime   = sd(crime_rate), 
   n          = n())

communities |>
  group_by(disadvantage) |> #<<
  summarize(mean_crime = mean(crime_rate),
            sd_crime = sd(crime_rate), 
            n = n())

cor(communities$pop_density, communities$crime_rate)

with(communities, cor(pop_density, crime_rate))

communities |> summarize(R = cor(pop_density, crime_rate))

communities |>
  group_by(disadvantage) |>
  summarize(R = cor(pop_density, crime_rate))
