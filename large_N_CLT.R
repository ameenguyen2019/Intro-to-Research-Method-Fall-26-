rm(list = ls())

num_flips <- 10 ## Keep increasing by * 10 to see how plot changes

coin <- c(0, 1)

flips <- sample(coin, size = num_flips, replace = TRUE)
flips
mean(flips)

heads_freq <- cumsum(flips == 0) / 1:num_flips

heads_freq[(length(heads_freq)-num_flips):length(heads_freq)]

plot(heads_freq, type="l", bty="l", col="maroon", lwd=2)
abline(h=0.5)


## Central limit theorem example:

# create random variable with highly skewed distribution

x <- rexp(500, 1/700)
hist(x)

sample_means <- NULL
for (i in 1:1000) {
  sample <- sample(x, 50)
  sample_means[i] <- mean(sample)
}
hist(sample_means, breaks=10)

# With uniform distribution

x <- runif(500, 0, 1)
hist(x)

sample_means <- NULL
for (i in 1:10000) {
  sample <- sample(x, 50)
  sample_means[i] <- mean(sample)
}
hist(sample_means, breaks=10)



