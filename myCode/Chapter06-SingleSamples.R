# Chapter 06 - Single Samples
data <- read.csv("./data/example.csv")
names(data)
summary(data)

boxplot(data)
plot(seq(1,nrow(data),1), data$y)
hist(data$y)

# the normal distribution
score <- 2:12
ways <- c(1,2,3,4,5,6,5,4,3,2,1)
game <- rep(score, ways)

# play the game 10,000 times and plot the score
outcome <- numeric(10000)
for (i in 1:10000) outcome[i] <- sample(game, 1)
hist(outcome, breaks = 1.5:12.5)
mean(outcome)

mean.outcome <- numeric(10000)
for (i in 1:10000) mean.outcome[i] <- mean(sample(game, 3))
hist(mean.outcome, breaks = 1.5:12.5)

# overlay the density function
xv <- seq(2,12,0.1)
yv <- 10000 * dnorm(xv, mean(mean.outcome), sd(mean.outcome))
hist(mean.outcome, breaks = (1.5:12.5), ylim = c(0,3000), col = "yellow", main = "")
lines(xv,yv,col="red")

standard.deviations <- seq(-3,3, 0.01)
pd <- dnorm(standard.deviations)
plot(standard.deviations, pd, type = "l", col = "blue")

# Using z: Average Height = 170, Standard Deviation of Height = 8
ht <- seq(150,190,0.01)
plot(ht, dnorm(ht, mean = 170, sd = 8), type = "l", col = "brown",
     ylab = "Probability Density", xlab = "Height")

# playing with polygon for graphing
xv <- seq(0, 10, 0.01)
yv <- seq(0, 10, 0.01)
plot(xv, yv, type = "n")
x <- c(2,6,8)
y <- c(2,2,8)
polygon(x,y)

# use of polygon for coloring
par(mfrow=c(2,2))
ht <- seq(150,190,0.01)
pd <- dnorm(ht, mean = 170, sd = 8)
# top left plot
plot(ht, dnorm(ht, mean = 170, sd = 8), type = "l", col = "brown",
     ylab = "Probability Density", xlab = "Height")
# top right plot
plot(ht, dnorm(ht, mean = 170, sd = 8), type = "l", col = "brown",
     ylab = "Probability Density", xlab = "Height")
yv <- pd[ht <= 160]
xv <- ht[ht<=160]
xv <- c(xv, 160, 150)
yv <- c(yv, yv[1], yv[1])
polygon(xv, yv, col = "orange")
# bottom left graph
plot(ht, dnorm(ht, mean = 170, sd = 8), type = "l", col = "brown",
     ylab = "Probability Density", xlab = "Height")
xv <- ht[ht >= 185]
yv <- pd[ht >= 185]
xv <- c(xv, 190, 185)
yv <- c(yv, yv[501], yv[501])
polygon(xv, yv, col = "blue")
# bottom left graph
plot(ht, dnorm(ht, mean = 170, sd = 8), type = "l", col = "brown",
     ylab = "Probability Density", xlab = "Height")
xv <- ht[ht >= 160 & ht <= 180]
yv <- pd[ht >= 160 & ht <= 180]
xv <- c(xv, 180, 160)
yv <- c(yv, pd[1], pd[01])
polygon(xv, yv, col = "green")
par(mfrow=c(1,1))

# Testing normality
data <- read.csv("./data/skewdata.csv")
names(data)
qqnorm(data$values)
qqline(data$"values", lty=2)
