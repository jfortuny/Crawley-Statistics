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
