plot(c(0,30), c(0,60), type = "n", xlab = "Sample Size", ylab = "Confidence Interval")

for (k in seq(5, 30, 3)){
  a <- numeric(10000)
  for (i in 1:10000) {
    a[i] <- mean(sample(skewdata$values, k, replace = TRUE))
  }
  points(c(k,k), quantile(a, c(0.025, 0.975)), type = "b", pch = 21, bg = "red")
}

plot(c(0,60), c(0,60), type = "n", xlab = "Sample Size", ylab = "Confidence Interval")

for (k in seq(5, 60, 3)){
  a <- numeric(10000)
  for (i in 1:10000) {
    a[i] <- mean(sample(skewdata$values, k, replace = TRUE))
  }
  points(c(k,k), quantile(a, c(0.025, 0.975)), type = "b", pch = 21, bg = "red")
}