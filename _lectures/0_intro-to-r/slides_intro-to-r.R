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

string.vector <- c("Atlantic", "Pacific", "Arctic")
string.vector

factor.vector <- factor(string.vector)
factor.vector

save(new.object, file="new_object.RData")

load("new_object.RData")

getwd()

## setwd("C:/Users/")

data(USArrests)

head(USArrests, 5) # prints first 5 rows, see tail() too

str(USArrests) # str[ucture]

summary(USArrests)

USArrests[1,] # First row

USArrests[1:3, 3:4] # First three rows, third and fourth column #<<

USArrests$UrbanPop[1:10]

USArrests[USArrests$Murder > 15, ]

USArrests$Murder > 15

vector_w_missing <- c(1, 2, NA, 4, 5, 6, NA)

mean(vector_w_missing)

mean(vector_w_missing, na.rm=TRUE)

vector_w_missing == NA

is.na(vector_w_missing)

mean(vector_w_missing[!is.na(vector_w_missing)]) #<<

## library(swirl)
