# Chapter 6 - Two samples

# Comparing two variances - F test
f.test.data <- read.csv("./data/f.test.data.csv")
names(f.test.data)
var(f.test.data$gardenB)
var(f.test.data$gardenC)
F.ratio <- var(f.test.data$gardenC) / var(f.test.data$gardenB)
# test
qf(0.975,9,9)
# p value
2 * (1 - pf(F.ratio, 9, 9))
# reject the null hypotheses that the two gardens have the same variance

# analysisin one step
var.test(f.test.data$gardenB, f.test.data$gardenC)

# Comparing two means - t test
t.test.data <- read.csv("./data/t.test.data.csv")
names(t.test.data)
mean(t.test.data$gardenA)
mean(t.test.data$gardenB)
ozone <- c(t.test.data$gardenA, t.test.data$gardenB)
label <- factor(c(rep("A",10), rep("B",10)))
boxplot(ozone~label, notch=TRUE, xlab="Garden", ylab="Ozome pphm", col="lightblue")                

# carrying out the test step-by-step
s2A <- var(t.test.data$gardenA)
s2B <- var(t.test.data$gardenB)
# are the variances significantly different?
s2A/s2B
# proceed  with t test
(mean(t.test.data$gardenA) - mean(t.test.data$gardenB)) / sqrt(s2A/10 + s2B/10)
qt(0.975, 18)
# p value
2 * pt((mean(t.test.data$gardenA) - mean(t.test.data$gardenB)) / sqrt(s2A/10 + s2B/10), 18)
# alternative: direct t-test
t.test(t.test.data$gardenA, t.test.data$gardenB)
# alternative non-parametric: Wilcoxon Rank-Sum
combined.ranks <- rank(ozone)
tapply(combined.ranks, label, sum)
# or
wilcox.test(t.test.data$gardenA, t.test.data$gardenB)

# Correlation and Covariance
data <- read.csv("./data/twosample.csv")
plot(data$x, data$y, pch=21, col="blue", bg="orange")

# Calculation by hand
var(data$x, data$y) / sqrt(var(data$x) * var(data$y))
# by formula
cor(data$x, data$y)


# Scale-dependent correlations
data <- read.csv("./data/productivity.csv")
head(data)
plot(data$productivity, data$mammals, pch=16, col="blue")
cor.test(data$productivity, data$mammals, method = "spearman")

# what is the effect of region in correlation
plot(data$productivity, data$mammals, pch=16, col=as.numeric(data$region))
