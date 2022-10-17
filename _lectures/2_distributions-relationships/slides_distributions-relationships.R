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
library(dplyr)

getwd()

shootings <- 
  read_csv( 
    "https://clanfear.github.io/ioc_iqa/_data/fatal-police-shootings-data.csv"
    )


glimpse(shootings)

sort(table(shootings$race))

shootings$race |> table() |> sort()

shootings |> filter(armed == "unarmed") |> head()

shootings |> slice(c(1, 5, 9))

## shootings |>
##     filter(armed == "unarmed" &
##              age < 18)

## shootings |>
##     filter(armed == "unarmed" |
##           age < 18)

metro_2021 <- read_csv("https://clanfear.github.io/ioc_iqa/_data/metro_2021.csv")

glimpse(metro_2021)

westminster <- metro_2021 |> filter(borough == "Westminster")
head(westminster, 4)

## plot(robbery ~ month,
##      data = westminster,
##      xlab = "Month",
##      ylab = "Robbery",
##      main = "Robbery in Westminster in 2021",
##      col = "red",
##      cex.lab = 1.5,
##      cex.main= 1.5,
##      pch = 16)
