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

metro_2021 |> count(subregion)

metro_2021 |> count(subregion) |> mutate(Proportion = n/sum(n))

metro_2021 |> 
  filter(month == 6) |>
  summarise(n_obs = n(),
            mean_robbery = mean(robbery),
            sd_robbery = sd(robbery))

metro_2021 |> 
  filter(borough == "Westminster") |>
  summarise(n_obs = n(),
            mean_burglary = mean(burglary),
            sd_burglary = sd(burglary))

metro_2021 |>
  group_by(month) |> #<<
    summarise(n_obs = n(),
            mean_robbery = mean(robbery),
            sd_robbery = sd(robbery)) |>
    head(5)

metro_2021 |> 
  group_by(borough) |>
  summarise(n_obs = n(),
            mean_burglary = mean(burglary),
            sd_burglary = sd(burglary)) |>
  head()

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
        aes(x = month, y = robbery)) +
  geom_point()

ggplot(data = westminster,  #<<
       aes(x = month, y = robbery)) #<<

ggplot(data = westminster, 
      aes(x = month, y = robbery)) +
    geom_point() #<<

ggplot(data = westminster, 
      aes(x = month, y = robbery)) +
  geom_point(color = "red", size = 3) #<<

ggplot(data = westminster, 
      aes(x = month, y = robbery)) +
  geom_point(color = "red", size = 3) +
  xlab("Month") #<<

ggplot(data = westminster, 
      aes(x = month, y = robbery)) +
  geom_point(color = "red", size = 3) +
  xlab("Month") + 
  ylab("Robbery") #<<

ggplot(data = westminster, 
      aes(x = month, y = robbery)) +
  geom_point(color = "red", size = 3) +
  xlab("Month") + 
  ylab("Robbery") +
   ggtitle("Robbery in Westminster") #<<

ggplot(data = westminster, 
      aes(x = month, y = robbery)) +
  geom_point(color = "red", size = 3) +
  xlab("Month") + 
  ylab("Robbery") +
  ggtitle("Robbery in Westminster") +
  theme_bw() #<<

 ggplot(data = westminster, 
      aes(x = month, y = robbery)) +
  geom_point(color = "red", size = 3) +
  xlab("Month") + 
  ylab("Robbery") +
  ggtitle("Robbery in Westminster") +
   theme_bw(base_size=18) #<<

 ggplot(data = metro_2021, 
      aes(x = month, y = robbery)) +
  geom_point(color = "red", size = 3) +
  xlab("Month") + 
  ylab("Robbery") +
  ggtitle("Robbery in London") +
   theme_bw(base_size=18) #<<

 ggplot(data = metro_2021, 
      aes(x = month, y = robbery)) +
  geom_line(color = "red", size = 3) + #<<
 xlab("Month") + 
  ylab("Robbery") +
  ggtitle("Robbery in London") +
  theme_bw(base_size=18)

 ggplot(data = metro_2021, 
      aes(x = month, y = robbery, group = borough)) +
  geom_line(color = "red", size = 3) + #<<
 xlab("Month") + 
  ylab("Robbery") +
  ggtitle("Robbery in London") +
  theme_bw(base_size=18)

 ggplot(data = metro_2021, 
      aes(x = month, y = robbery, group = borough)) +
  geom_line(color = "red") + #<<
 xlab("Month") + 
  ylab("Robbery") +
  ggtitle("Robbery in London") +
  theme_bw(base_size=18)

 ggplot(data = metro_2021, 
      aes(x = month, y = robbery, group = borough,
          color = subregion)) + #<<
  geom_line() + 
 xlab("Month") + 
  ylab("Robbery") +
  ggtitle("Robbery in London") +
  theme_bw(base_size=18)

 ggplot(data = metro_2021, 
      aes(x = month, y = robbery, group = borough,
          color = subregion)) + #<<
  geom_line() + 
  facet_wrap(~subregion) +
  xlab("Month") + 
  ylab("Robbery") +
  ggtitle("Robbery in London") +
  theme_bw(base_size=18)

 ggplot(data = metro_2021, 
      aes(x = month, y = robbery, group = borough,
          color = subregion)) + #<<
  geom_line() + 
  facet_wrap(~subregion) +
  xlab("Month") + 
  ylab("Robbery") +
  ggtitle("Robbery in London") +
  theme_bw(base_size=18) +
  theme(legend.position = "none")
