# Chapter 7 - Linear Regression

reg.data <- read.csv("./data/tannin.csv")
names(reg.data)
plot(reg.data$tannin, reg.data$growth, pch=21, bg="blue")

lm(reg.data$growth~reg.data$tannin)
abline(lm(reg.data$growth~reg.data$tannin), col="green")

(fitted <- predict(lm(reg.data$growth~reg.data$tannin)))
for (i in 1:9) {
  lines(c(reg.data$tannin[i], reg.data$tannin[i]),
        c(reg.data$growth[i], fitted[i]), col="red")
}

# Graphing and analyzing the model
model <- lm(reg.data$growth~reg.data$tannin)
summary(model)
summary.aov(model)
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
influence.measures(model)
