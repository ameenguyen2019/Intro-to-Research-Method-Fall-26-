######## Chap 7-Implementation of Uncertainty #########
## simulation parameters
n <- 100 # sample size
mu0 <- 0 # mean of Y_i(0)
sd0 <- 1 # SD of Y_i(0)
mu1 <- 1 # mean of Y_i(1)
sd1 <- 1 # SD of Y_i(1)
## generate a sample
y0 <- rnorm(n, mean = mu0, sd = sd0) # randomly draw a sample from a normal distribution
y1 <- rnorm(n, mean = mu1, sd = sd1)
tau <- y1 - y0 # individual treatment effect
## true value of the sample average treatment effect
sate <- mean(tau)
sate
## repeatedly conduct randomized controlled trials
sims <- 5000 # repeat 5000 times
diff.means <- rep(NA, sims) # container
# use loop to stimulate a large # of RCT, randomly assign treatment to units, select one of potential outcomes
for (i in 1:sims) {
  ## randomize the treatment by sampling of a vector of 0s and 1s
  treat <- sample(c(rep(1, n/2), rep(0, n/2)), size = n, replace = F)
  ## difference-in-means
  diff.means <- mean(y1[treat == 1]) - mean(y0[treat == 0])
}
## estimation error for SATE
est.error <- diff.means - sate
summary(est.error)
## PATE simulation
pate <- mu1 - mu0
diff.means <- rep(NA, sims)
for (i in 1:sims) {
  ## generate a sample for each simulation: this used to be outside loop
  y0 <- rnorm(n, mean = mu0, sd = sd0)
  y1 <- rnorm(n, mean = mu1, sd = sd1)
  treat <- sample(c(rep(1, n/2), rep(0, n/2)), size = n, replace = F)
  diff.means[i] <- mean(y1[treat == 1]) - mean(y0[treat == 0])
}
## estimation error for PATE
est.error <- diff.means - pate
summary(est.error)

## standard error
# plot dif-in-means estimator
hist(diff.means, freq = FALSE,xlab = "Difference-in-means estimator",
     main = "Sampling distribution")
abline(v = sate, col = "blue") # true value of SATE
# tandard deviation of the sampling
# distribution of the dif-in-means estimator
sd(diff.means)
sqrt(mean(diff.means - sate)^2) # 0.19

### Standard error
## PATE simulation with standard error
sims <- 5000
diff.means <- se <- rep(NA, sims) # container for SE added
for (i in 1:sims) {
  ## generate a sample
  y0 <- rnorm(n, mean = mu0, sd = sd0)
  y1 <- rnorm(n, mean = mu1, sd = sd1)
  ## randomize the treatment by sampling of a vector of 0s and 1s
  treat <- sample(c(rep(1, n/2), rep(0, n/2)), size = n, replace = F)
  diff.means[i] <- mean(y1[treat == 1]) - mean((y0[treat == 0]))
  ## SE 
  se[i] <- sqrt(var(y1[treat == 1]) / (n/2) + var(y0[treat == 0]) / (n/2))
}
## SD of difference-in-means
sd(diff.means) # 0.2
## mean of SE(s)
mean(se) #0.20

## 99%, 95%, and 90% confidence intervals can be computed as follows
n <- 1000 # sample size
x.bar <- 0.6 #point estimate/ pop mean
s.e. <- sqrt(x.bar * (1 - x.bar) / n) # SE
# 99% CI
c(x.bar - qnorm(0.995) * s.e., x.bar + qnorm(0.995) * s.e.)
# 95% CI
c(x.bar - qnorm(0.975) * s.e., x.bar + qnorm(0.975) * s.e.)
# 90% CI
c(x.bar - qnorm(0.95) * s.e., x.bar + qnorm(0.95) * s.e.)

## conduct Monte simulation, compute 90%, 95%CI for 5000 simulations
## empty container matrices for 2 sets of confidence intervals
ci95 <- ci90 <- matrix(NA, ncol = 2, nrow = sims)
# 95% CI
ci95[, 1] <- diff.means - qnorm(0.975) * se # lower limit, All rows in column 1
ci95[, 2] <- diff.means + qnorm(0.975) * se # upper limit, All rows in column 2
# 90% 
ci90[, 1] <- diff.means - qnorm(0.95) * se # lower limit
ci90[, 2] <- diff.means + qnorm(0.95) * se # upper limit
## coverage rate for 95% confidence interval
mean(ci95[, 1] <= 1 & ci95[, 2] >= 1) # 0.946
## coverage rate for 90% confidence interval
mean(ci90[, 1] <= 1 & ci90[, 2] >= 1) # 0.892
#### therefore, CIs are valid, bc they contain true values of true parameter,
#### which is equal to 1

# another example of polling
p <- 0.6 # true parameter value
n <- c(10, 100, 1000) # 3 sample sizes to be examined
alpha <- 0.05
sims <- 5000 # of simulations
results <- rep(NA, length(n)) # a container for results
## loop for different sample sizes
for (i in 1:length(n)) {
  ci.results <- rep(NA, sims) # a container for whether CI includes truth
  ## loop for repeated hypothetical survey sampling
  for (j in 1:sims) {
    data <- rbinom(n[i], size = 1, prob = p) # simple random sampling
    x.bar <- mean(data) # sample proportion as an estimate
    s.e. <- sqrt(x.bar * (1 - x.bar) / n[i]) # standard errors
    ci.lower <- x.bar - qnorm(1 - alpha / 2) * s.e.
    ci.upper <- x.bar + qnorm(1 - alpha / 2) * s.e.
    ci.results[j] <- (p >= ci.lower) & (p <= ci.upper)
  }
  ## proportion of CIs that contain the true value
  results[i] <- mean(ci.results)
}
results

### MOE
moe <- c(0.01, 0.03, 0.05) # the desired margin of error
p <- seq(from = 0.01, to = 0.99, by = 0.01)
n <- 1.96^2 * p * (1 - p) / moe[1]^2
plot(p, n, ylim = c(-1000, 11000), xlab = "Population proportion", 
     ylab = "Sample size", type = "l")
lines(p, 1.96^2 * p * (1 - p) / moe[2]^2, lty = "dashed")
lines(p, 1.96^2 * p * (1 - p) / moe[3]^2, lty = "dotted")
text(0.5, 10000, "margin of error = 0.01")
text(0.5, 1800, "margin of error = 0.03")
text(0.5, -200, "margin of error = 0.05")

# analyze 2008 presidential election polling data, computed our estimated
# margin of victory for Obama within each state
## election and polling results, by state
pres08 <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 5/PREDICTION/pres08.csv")
polls08 <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 5/PREDICTION/polls08.csv")
## convert to a Date object
polls08$middate <- as.Date(polls08$middate)
## number of days to Election Day
polls08$daystoelection <- as.Date("2008-11-04") - polls08$middate
## create a matrix place holder
poll.pred <- matrix(NA, nrow = 51, ncol = 3)
## state names which the loop will iterate through
st.names <- unique(pres08$state)
## add labels for easy interpretation later on
row.names(poll.pred) <- as.character(st.names)
## loop across 50 states plus DC
for (i in 1:51) {
  ## subset the ith state
  state.data <- subset(polls08, subset = (state == st.names[i]))
  ## subset the latest polls within the state
  latest <- state.data$daystoelection == min(state.data$daystoelection)
  ## compute the mean of latest polls and store it
  poll.pred[i, 1] <- mean(state.data$Obama[latest]) / 100
}
## upper and lower confidence limits
n <- 1000 # sample size
alpha <- 0.05
s.e. <- sqrt(poll.pred[, 1] * (1 - poll.pred[, 1]) / n) #SE
poll.pred[, 2] <- poll.pred[, 1] - qnorm(1 - alpha / 2) * s.e.
poll.pred[, 3] <- poll.pred[, 1] + qnorm(1 - alpha / 2) * s.e.

# compare the polling prediction of Obama’s support share (the vertical axis)
# against Obama’s vote share on Election Day (the horizontal axis)
alpha <- 0.05
plot(pres08$Obama / 100, poll.pred[, 1], xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Obama's vote share", ylab = "Poll prediction")
abline(0, 1)
## adding 95% CI for each state
for (i in 1:51) {
  lines(rep(pres08$Obama[i] / 100, 2), c(poll.pred[i, 2], poll.pred[i, 3]))
}
## prop of CI that contain the Election Day outcome
mean((poll.pred[, 2] <= pres08$Obama / 100) & poll.pred[, 3] >= pres08$Obama / 100) # 0.588
## coverage rate is 58.8%, CI isn't centered around true parameter value
# compute bias & alter SE
# bias
bias <- mean(poll.pred[, 1] - pres08$Obama / 100)
bias
## bias corrected estimate
poll.bias <- poll.pred[, 1] - bias
# examine the coverage rate of the bias-corrected CI
# bias-corrected SE 
s.e.bias <- sqrt(poll.bias * (1 - poll.bias) / n)
# bias-corrected 95% CI
ci.bias.lower <- poll.bias - qnorm(0.975) * s.e.bias
ci.bias.upper <- poll.bias + qnorm(0.975) * s.e.bias
## prop of bias-corrected CIs that contain the Election Day outcome
mean((ci.bias.lower <= pres08$Obama / 100) & ci.bias.upper >= pres08$Obama / 100) #0.764
# The bias correction improves the coverage rate by almost 20 pct pts
#  still far from the nominal coverage rate of 95%

### Analysis of RCTs: randomly assigned to a small class, regular class, or regular class with an aid. 
### We are interested in knowing whether small class size improves students’ test performance
star <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 2/CAUSALITY/STAR.csv", head = TRUE)
View(star)
# plot DV for small and regular classes
hist(star$g4reading[star$classtype == 1], freq = F, xlim = c(500, 900),
     ylim = c(0, 0.01), main = "Small class",
     xlab = "Fourth-grade reading test score")
# add average score for each group
abline(v = mean(star$g4reading[star$classtype == 1], na.rm = TRUE), col = "blue")
hist(star$g4reading[star$classtype == 2], freq = F, xlim = c(500, 900),
     ylim = c(0, 0.01), main = "Regular class",
     xlab = "Fourth-grade reading test score")
abline(v = mean(star$g4reading[star$classtype == 2], na.rm = TRUE), col = "red")
## estimate and SE for small class
n.small <- sum(star$classtype == 1 & !is.na(star$g4reading)) # remove missing obs
est.small <- mean(star$g4reading[star$classtype == 1], na.rm = T)
se.small <- sd(star$g4reading[star$classtype == 1], na.rm = T) / sqrt(n.small)
est.small # 723.39
se.small # 1.91

## estimate and standard error for regular class
n.regular <- sum(star$classtype == 2 & !is.na(star$classtype) & !is.na(star$g4reading))
est.regular <- mean(star$g4reading[star$classtype == 2], na.rm = T)
se.regular <- sd(star$g4reading[star$classtype == 2], na.rm = T) / sqrt(n.regular)
est.regular # 719.89
se.regular #1.83

# construct 95% CI
alpha <- 0.05
# 95% CI for small class
ci.small <- c(est.small - qnorm(0.975) * se.small, est.small + qnorm(0.975) * se.small)
ci.small #[719.6417, 727.1406]
# 95% CI for regular class
ci.regular <- c(est.regular - qnorm(0.975) * se.regular, est.regular + qnorm(0.975) * se.regular)
ci.regular # [716.2859, 723.4940]
# these CIs overlapping
# is this stat sig? Compute CI for the estimated average difference
## difference-in-means estimator
ate.est <- est.small - est.regular
ate.est # 3.501
# SE & 95% CI
ate.se <- sqrt(se.small^2 + se.regular^2)
ate.se # 2.65
ate.ci <- c(ate.est - qnorm(0.975) * ate.se, ate.est + qnorm(0.975) * ate.se)
ate.ci # [-1.699503,  8.701968]
##### the average treatment effect of small class size on the fourth-grade
#### reading score is estimated to be 3.50 with a standard error of 2.65
#### 95% CI is [−1.70, 8.70], containing zero.
##### suggests that although the estimated average treatment effect is positive, 
#### it features a considerable degree of uncertainty

### H-Testing
### Tea-testing experiment
## truth: enumerate the number of assignment combinations
true <- c(choose(4, 0) * choose(4, 4),
          choose(4, 1) * choose(4, 3),
          choose(4, 2) * choose(4, 2),
          choose(4, 3) * choose(4, 1),
          choose(4, 4) * choose(4, 0))
true
## compute probability: divide it by the total number of events
true <- true / sum(true)
## number of correctly classified cups as labels
names(true) <- c(0, 2, 4, 6, 8)
true
## simulations
sims <- 1000
## lady’s guess: M stands for “milk first,” T stands for “tea first”
guess <- c("M", "T", "T", "M", "M", "T", "T", "M")
correct <- rep(NA, sims) # place holder for number of correct guesses
for (i in 1:sims) {
  ## randomize which cups get milk/tea first
  cups <- sample(c(rep("T", 4), rep("M", 4)), replace = F)
  correct[i] <- sum(guess == cups) # # of correct guesses
}
## estimated probability for each number of correct guesses
prop.table(table(correct))
## comparison with analytical answers; the differences are small
prop.table(table(correct)) - true

### Fisher's test for tea-testing experiment
## all correct
x <- matrix(c(4, 0, 0, 4), byrow = TRUE, ncol = 2, nrow = 2)
## 6 correct
y <- matrix(c(3, 1, 1, 3), byrow = TRUE, ncol = 2, nrow = 2)
## “M” milk first, “T” tea first
rownames(x) <- colnames(x) <- rownames(y)<- colnames(y) <- c("M", "T")
x
y
# conduct Fisher’s exact test with one-sided and two-sided alternatives
## one-sided test for 8 correct guesses
fisher.test(x, alternative = "greater")
## two-sided test for 6 correct guesses
fisher.test(y)

### 2-sided H-testing for Obama's vote support
n <- 1018
x.bar <- 550 / n
se <- sqrt(0.5 * 0.5 / n) # SD of sampling distribution
## upper blue area in the figure
upper <- pnorm(x.bar, mean = 0.5, sd = se, lower.tail = F)
## lower blue area in the figure; identical to the upper area
lower <- pnorm(0.5 - (x.bar - 0.5), mean = 0.5, sd = se)
## two-sided p-value
upper + lower # 0.01 => reject H0
2 * upper # 0.01
## one-sided p-value
upper # 0.005 => reject H0

### compute z-score, if z > 1.96, reject H0
z.score <- (x.bar - 0.5) / se
z.score #2.57004
pnorm(z.score, lower.tail = FALSE) # one-sided p-value, 0.005
2 * pnorm(z.score, lower.tail = FALSE) # two-sided p-value, 0.01
## 99% confidence interval contains 0.5
c(x.bar - qnorm(0.995) * se, x.bar + qnorm(0.995) * se) # [0.4999093, 0.5806408]
## 95% confidence interval does not contain 0.5
c(x.bar - qnorm(0.975) * se, x.bar + qnorm(0.975) * se) # [0.5095605, 0.5709896]

## no continuity correction to get the same p-value as above
prop.test(550, n = n, p = 0.5, correct = FALSE) # one line of R code, p = 0.01
##
## 1-sample proportions test without continuity
## correction
##
## data: 550 out of n, null probability 0.5
## X-squared = 6.6051, df = 1, p-value = 0.01017
## alternative hypothesis: true p is not equal to 0.5

## 95 percent confidence interval:
# 0.5095661 0.5706812
## sample estimates: p = 0.54
## with continuity correction
prop.test(550, n = n, p = 0.5)
## 1-sample proportions test with continuity correction
##
## data: 550 out of n, null probability 0.5
## X-squared = 6.445, df = 1, p-value = 0.01113
## alternative hypothesis: true p is not equal to 0.5
## 95 percent confidence interval:
## 0.5090744 0.5711680
## sample estimates:
## p
## 0.540275

# yield CIs
prop.test(550, n = n, p = 0.5, conf.level = 0.99)
## 1-sample proportions test with continuity correction
## data: 550 out of n, null probability 0.5
## X-squared = 6.445, df = 1, p-value = 0.01113
## alternative hypothesis: true p is not equal to 0.5
## 99 percent confidence interval:
## 0.4994182 0.5806040
## sample estimates:
## p
## 0.540275

# use t-test to compute p-value for the null hypothesis that the population mean of
# test score is 710
## two-sided one-sample t-test
t.test(STAR$g4reading, mu = 710)
##
## One Sample t-test
##
## data: STAR$g4reading
## t = 10.407, df = 2352, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 710
## 95 percent confidence interval:
## 719.1284 723.3671
## sample estimates:
## mean of x
## 721.2478

### two-sample t test
## one-sided p-value
pnorm(-abs(ate.est), mean = 0, sd = ate.se) # 0.09
## two-sided p-value
2 * pnorm(-abs(ate.est), mean = 0, sd = ate.se) # 0.18
## testing the null of zero average treatment effect
t.test(star$g4reading[star$classtype == 1],
       star$g4reading[star$classtype == 2])
## Welch Two Sample t-test
##
## data:STAR$g4reading[STAR$classtype==1] and STAR$g4reading[STAR$classtype == 2]
## t = 1.3195, df = 1541.2, p-value = 0.1872
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
## -1.703591 8.706055
## sample estimates:
## mean of x mean of y
## 723.3912 719.8900

## two-sample z test
resume <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 11/UNCERTAINTY/resume.csv")
## organize the data in tables
x <- table(resume$race, resume$call)
x
## one-sided test
prop.test(x, alternative = "greater")
## 2-sample test for equality of proportions with
## continuity correction
##
## data: x
## X-squared = 16.449, df = 1, p-value = 2.499e-05
## alternative hypothesis: greater
## 95 percent confidence interval:
## 0.01881967 1.00000000
## sample estimates:
## prop 1 prop 2
## 0.9355236 0.9034908

## sample size
n0 <- sum(resume$race == "black")
n1 <- sum(resume$race == "white")
## sample proportions
p <- mean(resume$call) # overall
p0 <- mean(resume$call[resume$race == "black"]) # black
p1 <- mean(resume$call[resume$race == "white"]) # white
## point estimate
est <- p1 - p0
est
## standard error
se <- sqrt(p * (1 - p) * (1 / n0 + 1 / n1))
se

## z-statistic
zstat <- est / se
zstat

# one-sided p-value
pnorm(-abs(zstat))
prop.test(x, alternative = "greater", correct = FALSE)
##
## 2-sample test for equality of proportions without
## continuity correction
##
## data: x
## X-squared = 16.879, df = 1, p-value = 1.992e-05
## alternative hypothesis: greater
## 95 percent confidence interval:
## 0.01923035 1.00000000
## sample estimates:
## prop 1 prop 2
## 0.9355236 0.9034908

## Power analysis
## set the parameters
n <- 250
p.star <- 0.48 # data-generating process
p <- 0.5 # null value
alpha <- 0.05
## critical value
cr.value <- qnorm(1 - alpha / 2)
## standard errors under the hypothetical data-generating process
se.star <- sqrt(p.star * (1 - p.star) / n)
## standard error under the null
se <- sqrt(p * (1 - p) / n)
## power
pnorm(p - cr.value * se, mean = p.star, sd = se.star) +
  pnorm(p + cr.value * se, mean = p.star, sd = se.star, lower.tail = FALSE)
## parameters
n1 <- 500
n0 <- 500
p1.star <- 0.05
p0.star <- 0.1
## overall callback rate as a weighted average
p <- (n1 * p1.star + n0 * p0.star) / (n1 + n0)
## standard error under the null
se <- sqrt(p * (1 - p) * (1 / n1 + 1 / n0))
## standard error under the hypothetical data-generating process
se.star <- sqrt(p1.star * (1 - p1.star) / n1 + p0.star * (1 - p0.star) / n0)
pnorm(-cr.value * se, mean = p1.star - p0.star, sd = se.star) +
  pnorm(cr.value * se, mean = p1.star - p0.star,
        sd = se.star, lower.tail = FALSE)
## [1] 0.85228

power.prop.test(n = 500, p1 = 0.05, p2 = 0.1, sig.level = 0.05)
##
## ##
## n = 500
## p1 = 0.05
# Two-sample comparison of proportions power calculation
## p2 = 0.1
## sig.level = 0.05
## power = 0.8522797
## alternative = two.sided

# two-sample test for proportions
power.prop.test(p1 = 0.05, p2 = 0.1, sig.level = 0.05, power = 0.9)
##Two-sample comparison of proportions power calculation
## n = 581.0821
## p1 = 0.05
## p2 = 0.1
## sig.level = 0.05
## power = 0.9
## alternative = two.sided
##
## NOTE: n is number in *each* group

# compute power for 1/2 sample t-test
power.t.test(n = 100, delta = 0.25, sd = 1, type = "one.sample")
## NOTE: n is number in *each* group
power.t.test(power = 0.9, delta = 0.25, sd = 1, type = "one.sample")
