# Sometimes important stuff is highlighted! #<<
7 * 49

## > (11-2
## +

123 + 456 + 789

sqrt(400)

## ?sqrt

new.object <- 144

new.object

new.object + 10
new.object + new.object
sqrt(new.object)

new.object <- c(4, 9, 16, 25, 36)
new.object

sqrt(new.object)

head(USArrests, 5) # prints first 5 rows, see tail() too

str(USArrests) # str[ucture]

summary(USArrests)

hist(USArrests$Murder)

hist(USArrests$UrbanPop)

hist(USArrests$Murder,
     xlab = "Homicide Rate", # X axis label
     main = "Homicide Rates of US States") # Title

( murder_mean  <- mean(USArrests$Murder) )
( pop_mean <- mean(USArrests$UrbanPop) )

plot(Murder ~ UrbanPop, data = USArrests, #<<
     xlab = "Urban Population (100k)",
     ylab = "Homicides (per 100k)",
     main = "Homicide and urban populations in the US",
     pch = 16) # Point size
abline(h = murder_mean, col = "firebrick")
abline(v = pop_mean, col = "cornflowerblue")

## library(pander) # loads pander, do once in your session
## pander(summary(cars), style = "rmarkdown") #<<

library(pander) # loads pander, do once in your session
pander(summary(cars), style = "rmarkdown")
