# bootstrap
data <- read.csv("./data/skewdata.csv")
names(data)

plot(c(0,30), c(0,60), type = "n", xlab = "Sample Size", ylab = "Confidence Interval")

for (k in seq(5, 30, 3)){
  a <- numeric(10000)
  for (i in 1:10000) {
    a[i] <- mean(sample(data$values, k, replace = TRUE))
  }
  points(c(k,k), quantile(a, c(0.025, 0.975)), type = "b", pch = 21, bg = "red")
}

plot(c(0,60), c(0,60), type = "n", xlab = "Sample Size", ylab = "Confidence Interval")

for (k in seq(5, 60, 3)){
  a <- numeric(10000)
  for (i in 1:10000) {
    a[i] <- mean(sample(data$values, k, replace = TRUE))
  }
  points(c(k,k), quantile(a, c(0.025, 0.975)), type = "b", pch = 21, bg = "red")
}

# compare bootstrap with t-test
xv <- seq(5, 30, 0.1)
yv <- mean(data$values) + 1.96*sqrt(var(data$values)/xv)
lines(xv, yv, col="blue")
yv <- mean(data$values) - 1.96*sqrt(var(data$values)/xv)
lines(xv, yv, col="blue")
# student's t
yv <- mean(data$values) - qt(0.975,xv-1)*sqrt(var(data$values)/xv)
lines(xv, yv, lty=2, col="green")
yv <- mean(data$values) + qt(0.975,xv-1)*sqrt(var(data$values)/xv)
lines(xv, yv, lty=2, col="green")