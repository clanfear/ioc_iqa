x <- c(3, 4, 5) # Create x
x

x[3] # Get the third element of x

x[c(2,3)] # Get the second and third elements of x

(X <- matrix(c(3,4,5,6), nrow = 2))

X[2,1]

x <- c(7, 11, 11, 13, 26)
sum(x)

(1/length(x)) * sum(x)

mean(x)

x[(length(x) + 1) / 2]

median(x)

table(x)

table(x)[table(x)==max(table(x))]

z <- c(2, 5, 3, 5, 95)
mean(z)

median(z)

(s2 <- sum((x - mean(x))^2) / (length(x) -1))
var(x)

sqrt(var(x))
sd(x)

plot(c(0,5), c(0,5), type = "n", xlab = "x", ylab = "y")
abline(a = 1, b = 0.5)

plot(c(0,5), c(0,5), type = "n", xlab = "x", ylab = "y")
abline(a = 3, b = 0.5)

plot(c(0,5), c(0,5), type = "n", xlab = "x", ylab = "y")
abline(a = 2, b = 0.5)

plot(c(0,5), c(0,5), type = "n", xlab = "x", ylab = "y")
abline(a = 2, b = 0.5)

plot(c(0,5), c(0,5), type = "n", xlab = "x", ylab = "y")
abline(a = 2, b = 0)

plot(c(0,5), c(0,5), type = "n", xlab = "x", ylab = "y")
abline(a = 2, b = 2)

curve(2 + 0.5*x + 0.25*x^2, from = -2, to = 2, ylab = "y")
