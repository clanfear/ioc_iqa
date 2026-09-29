7 * 49 # <1>

# > (11 - 2
# +

123 + 456 + 789

sqrt(400)

# ?sqrt

new.object <- 144

8 * (33 + 92) / 4

new.object

new.object + 10
new.object + new.object
sqrt(new.object)

new.object <- c(4, 9, 16, 25, 36)
new.object

sqrt(new.object) # <1>

string.vector <- c("Atlantic", "Pacific", "Arctic", "Pacific")
string.vector

factor.vector <- factor(string.vector)
factor.vector

save(new.object, file="new_object.RData")

load("new_object.RData")

getwd()

setwd("C:/Users/")
getwd()

data(USArrests) # <1>

head(USArrests, 5) # <2> 

str(USArrests) # str[ucture]

summary(USArrests)

# library(swirl)
