###### Chapter 6: Probability implementation ######
# create a new function of birthday: compute at least 2 ple have same birthday
birthday <- function(k) 
{
  logdenom <- k * log(365) + lfactorial(365 - k) # log denominator
  lognumber <- lfactorial(365) # log numerator
  ## P(at least 2 have same bday) = 1 - P(nobody has same bday)
  pr <- 1 - exp(lognumber - logdenom) # transform back using expotential
  return(pr)
}
k <- 1:50 # the total of people are 50
bday <- birthday(k) # call the function
names(bday) <- k # add lables
plot(k, bday, xlab = "Number of people", xlim = c(0, 50), ylim = c(0,1),
     ylab = "Probability that at least two\n people have same birthday")
abline(h = 0.5) # horizontal 0.5 line
bday[20:25]

# Compute Prob in simulations using for loop
k <- 23 # number of people or number of elements
sims <- 1000 # number of simulations
event <- 0 # counter, start at 0
for (i in 1:sims) {   # repeat the code inside the loop 1000 times
  days <- sample(1:365, k, replace = T). # Randomly assign birthdays, possible 1 through 365, select k = 23 birthdays
  days.unique <- unique(days) # unique birthdays, remove duplicates
  ## if there are duplicates, the number of unique birthdays
  ## will be less than the number of birthdays, which is “k”
  if (length(days.unique) < k) { # Checks whether the number of unique birthdays is less than the number of people.
    event <- event + 1 # If the condition is true, increase the counter by one.
  }
}
# fraction of trials where at least two bdays are the same
answer <- event / sims
answer

choose(84, 6) # compute 84C6 (combinations, the ways of pick 6 out of 84)
lchoose(84, 6) # compute on logarith scale

### Conditional probability
flvoters <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 9/PROBABILITY/FLVoters.csv")
View(flvoters)
dim(flvoters) # before removal of missing data
flvoters <- na.omit(flvoters)
dim(flvoters)

#compute marginal probability
margin.race <- prop.table(table(flvoters$race)) # for race
margin.race

margin.gender <- prop.table(table(flvoters$gender)) # for gender
margin.gender

# compute conditional prob of race given gender
prop.table(table(flvoters$race[flvoters$gender == "f"]))

# compute joint prob of race and gender (the prop of voters belong to specific racial and gender groups)
joint.p <- prop.table(table(race = flvoters$race, gender = flvoters$gender))
joint.p

# compute prob of being a black voter
rowSums(joint.p)
# compute prob of gender
colSums(joint.p)

## compute prob of more than 2 events
# create one more var of age
flvoters$age.group <- NA # initialize a var
flvoters$age.group[flvoters$age <= 20] <- 1
flvoters$age.group[flvoters$age > 20 & flvoters$age <= 40] <- 2
flvoters$age.group[flvoters$age > 40 & flvoters$age <= 60] <- 3
flvoters$age.group[flvoters$age > 60] <- 4

# joint prob of age group, race, and gender
joint3 <- prop.table(table(race = flvoters$race, age.group = flvoters$age.group, gender = flvoters$gender))
joint3
# marginal probabilities for age groups
margin.age <- prop.table(table(flvoters$age.group))
margin.age
## P(black and female | above 60)
joint3["black", 4, "f"] / margin.age[4]

# create a two-way joint prob for age & gender
joint2 <- prop.table(table(age.group = flvoters$age.group, gender = flvoters$gender))
joint2
# P(female & above 60)
joint2[4, "f"] 
# P(black | female & above 60)
joint3["black", 4, "f"] / joint2[4, "f"]

## Independence
# check if race & gender are independent using scatter plot
plot(c(margin.race * margin.gender["f"]), # product of marginal probs
     c(joint.p[, "f"]), # joint probs
     xlim = c(0, 0.4), ylim = c(0, 0.4),
     xlab = "P(race) * P(female)", ylab = "P(race and female)")
abline(0, 1) # 45-degree line

## Plot joint independence
plot(c(joint3[, 4, "f"]), # joint prob , x-value
     margin.race * margin.age[4] * margin.gender["f"], # product of marginals, y-value
     xlim = c(0, 0.3), ylim = c(0, 0.3), main = "Joint independence",
     xlab = "P(race and above 60 and female)", 
     ylab = "P(race) * P(above 60) * P(female)")
abline(0, 1)

# conditional independence given female
plot(c(joint3[, 4, "f"]) / margin.gender["f"], # joint prob given female, x-value
     # product of marginals
     (joint.p[, "f"] / margin.gender["f"]) * (joint2[4, "f"] / margin.gender["f"]), # y-value
     xlim = c(0, 0.3), ylim = c(0, 0.3), main = "Marginal independence",
     xlab = "P(race and above 60 | female)",
     ylab = "P(race | female) * P(above 60 | female)")
abline(0, 1)     

# Monty Hall problem, pick a door to win a car
sims <- 1000
doors <- c("goat", "goat", "car") # order doesn't matter
result.switch <- result.nonswitch <- rep(NA, sims)
for (i in 1:sims) {
  ## randomly choose the initial door
  first <- sample(1:3, size = 1) # size 1: random choose 1 element from vector
  result.nonswitch[i] <- doors[first]
  remain <- doors[-first] # remaining 2 doors
  ## Monty chooses one door w/ a goat
  monty <- sample((1:2) [remain == "goat"], size =1)
  result.switch[i] <- remain[-monty]
}
mean(result.nonswitch == "car") # 0.335
mean(result.switch == "car")  # 0.665                                       

### Predict race using surname & residece location
cnames <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 9/PROBABILITY/names.csv")
View(cnames)
dim(cnames)

# use match() function
x <- c("blue", "red", "yellow")
y <- c("orange", "blue")
# match x w/ y
match(x, y) # returns the 2nd position of the matching element in y
# match y w/ x
match(y, x) 3 # return the 1st position of matching element in x

# remove voters whose surnames don't appear in census's surname list
flvoters <- flvoters[!is.na(match(flvoters$surname, cnames$surname)), ]
dim(flvoters)

## compute the prop of voters whose race is correctly classified in each category
## or true positive rate for each racial group
# subset white voters from vote file
whites <- flvoters[flvoters$race == "white", ]
# match surname of each voter w/ the same surname
w.indx <- match(whites$surname, cnames$surname)
head(w.indx)
# compare predicted prob of 5 racial categories in vector vars, then extract the highest predict prob
# relevant vars
vars <- c("pctwhite", "pctblack", "pctapi", "pcthispanic", "pctothers")
mean(apply(cnames[w.indx, vars], 1, max) == cnames$pctwhite[w.indx]) # 0.95
# check if the highest predicted prob of that voter is same as predicted prob of being white 

# do the same for black
blacks <- flvoters[flvoters$race == "black", ]
b.indx <- match(blacks$surname, cnames$surname)
mean(apply(cnames[b.indx, vars], 1, max) == cnames$pctblack[b.indx]) # 0.16

# hispanic
hispanics <- flvoters[flvoters$race == "hispanic", ]
h.indx <- match(hispanics$surname, cnames$surname)
mean(apply(cnames[h.indx, vars], 1, max) == cnames$pcthispanic[h.indx]) # 0.84

# asian
asians <- flvoters[flvoters$race == "asian", ]
a.indx <- match(asians$surname, cnames$surname)
mean(apply(cnames[a.indx, vars], 1, max) == cnames$pctapi[a.indx]) # 0.56

## compute false positive rate for each racial group
# Match voters' surnames to the surname dataset
indx <- match(flvoters$surname, cnames$surname)
# white false positive rate (prop of voters who aren't white among those classified as white)
1 - mean(flvoters$race[apply(cnames[indx, vars], 1, max) == cnames$pctwhite[indx]] == "white") # 0.20

# black false positive rate
1 - mean(flvoters$race[apply(cnames[indx, vars], 1, max) == cnames$pctblack[indx]] == "black") # 0.33

# hispanic false positive rate
1 - mean(flvoters$race[apply(cnames[indx, vars], 1, max) == cnames$pcthispanic[indx]] == "hispanic") # 0.23

# asian false positive rate
1 - mean(flvoters$race[apply(cnames[indx, vars], 1, max) == cnames$pctapi[indx]] == "asian") # 0.34

### include residence location to improve the prediction of individual race
flcensus <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 9/PROBABILITY/FLCensusVTD.csv")
View(flcensus)
## compute proportions by applying weighted.mean() to each column: average of % of racial category across districts w/ population of distric
race.prop <- apply(flcensus[, c("white", "black", "api", "hispanic", "others")], 
                   2, weighted.mean, weights = flcensus$total.pop)
race.prop # race prop in Florida

# compute P(surname | race) = (P(race | surname) x P(surname)) / P(race) using cnames dataset
total.count <- sum(cnames$count)
## P(surname | race) = P(race | surname) * P(surname) / P(race)
cnames$name.white <- (cnames$pctwhite / 100) * (cnames$count / total.count) / race.prop["white"]
cnames$name.black <- (cnames$pctblack / 100) * (cnames$count / total.count) / race.prop["black"]
cnames$name.hispanic <- (cnames$pcthispanic / 100) * (cnames$count / total.count) / race.prop["hispanic"]
cnames$name.asian <- (cnames$pctapi / 100) * (cnames$count / total.count) / race.prop["api"]
cnames$name.others <- (cnames$pctothers / 100) * (cnames$count / total.count) / race.prop["others"]

## compute denominator P(surname | residence)
# merge census data into voter file using 'county' & 'VTD'
flvoters <- merge(x = flvoters, y = flcensus, by = c("county", "VTD"), all = F)
## P(surname | residence) = sum_race P(surname | race) x P(race | residence)
indx <- match(flvoters$surname, cnames$surname)
flvoters$name.residence <- cnames$name.white[indx] * flvoters$white + 
  cnames$name.black[indx] * flvoters$black +
  cnames$name.hispanic[indx] * flvoters$hispanic +
  cnames$name.asian[indx] * flvoters$api +
  cnames$name.others[indx] * flvoters$others

# plug quantities into equation to compute predicted prob that an individual belongs to a race, 
# given his surname and residence
## P(race | surname, residence) = P(surname | race) * P(race | residence) / P(surname | residence)
flvoters$pre.white <- cnames$name.white[indx] * flvoters$white / flvoters$name.residence
flvoters$pre.black <- cnames$name.black[indx] * flvoters$black / flvoters$name.residence
flvoters$pre.hispanic <- cnames$name.hispanic[indx] * flvoters$hispanic / flvoters$name.residence
flvoters$pre.asian <- cnames$name.asian[indx] * flvoters$api / flvoters$name.residence
flvoters$pre.others <- 1 - flvoters$pre.white - flvoters$pre.black - flvoters$pre.hispanic - flvoters$pre.asian

# examine true positive rates
## relevant vars
vars1 <- c("pre.white", "pre.black", "pre.hispanic", "pre.asian", "pre.others")
# white
whites <- flvoters[flvoters$race == "white", ]
mean(apply(whites[, vars1], 1, max) == whites$pre.white) # 0.94

# black
blacks <- flvoters[flvoters$race == "black", ]
mean(apply(blacks[, vars1], 1, max) == blacks$pre.black) # 0.65

# hispanic
hispanics <- flvoters[flvoters$race == "hispanic", ]
mean(apply(hispanics[, vars1], 1, max) == hispanics$pre.hispanic) # 0.86

# asian
asians <- flvoters[flvoters$race == "asian", ]
mean(apply(asians[, vars1], 1, max) == asians$pre.asian) # 0.61

## proportion of blacks among those with surname "White"
cnames$pctblack[cnames$surname == "WHITE"] # 27.38
## predicted probabilities of being Black among voters with the surname "WHITE"
summary(flvoters$pre.black[flvoters$surname == "WHITE"])

# compute the false positive rate for each race
# white
1 - mean(flvoters$race[apply(flvoters[, vars1], 1, max) == flvoters$pre.white] == "white") # 0.12
# black
1 - mean(flvoters$race[apply(flvoters[, vars1], 1, max) == flvoters$pre.black] == "black") # 0.23
# hispanic
1 - mean(flvoters$race[apply(flvoters[, vars1], 1, max) == flvoters$pre.hispanic] == "hispanic") # 0.22
# asian
1 - mean(flvoters$race[apply(flvoters[, vars1], 1, max) == flvoters$pre.asian] == "asian") # 0.35

### Bernoulli & uniform distribution
## uniform PDF: x = 0.5, interval = [0, 1]
dunif(0.5, min = 0, max = 1) # 1
## uniform CDF: x = 1, interval = [-2, 2]
punif(1, min = -2, max = 2) # 0.75
# use Monte Carlo simulation
sims <- 1000
p <- 0.5 # success prob
x <- runif(sims, min = 0, max = 1) #uniform [0,1]
head(x)
y <- as.integer(x <= p) # Bernoulli; turn TRUE/FALSE to 1/0
mean(y) # close to success probability p, proportion of 1s vs. 0s # 0.51

# compute binomial distribution
dbinom(2, size = 3, prob = 0.5)

# compute the CDF of a binomial distribution in R using the pbinom() function
## CDF when x = 1, n = 3, p = 0.5
pbinom(1, size = 3, prob = 0.5) # 0.5
## number of voters who turn out
voters <- c(1000, 10000, 100000)
dbinom(voters / 2, size = voters, prob = 0.5) # then evaluate the PMF at exactly half of all voters who turn out.

## plus minus 1 standard deviation from the mean
pnorm(1) - pnorm(-1)
## plus minus 2 standard deviations from the mean
pnorm(2) - pnorm(-2)
# we can speficy mean without transformin a var into a standard normal var
mu <- 5
sigma <- 2
## plus minus 1 standard deviation from the mean
pnorm(mu + sigma, mean = mu, sd = sigma) - pnorm(mu - sigma, mean = mu, sd = sigma)
## plus minus 2 standard deviations from the mean
pnorm(mu + 2*sigma, mean = mu, sd = sigma) - pnorm(mu - 2*sigma, mean = mu, sd = sigma)

# use Bernoull draws earlier, compute sample variance
# theoretical variance: p was set to 0.5 earlier
p * (1-p) # 0.25
## sample variance using “y” generated earlier
var(y).  # 0.25

#### Predict election outcomes w/ uncertainty
pres08 <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 9/PROBABILITY/pres08.csv")
## two-party vote share
pres08$p <- pres08$Obama / (pres08$Obama + pres08$McCain)
# interview 1000 voters, use Monte simulation. For each success prob, function returns
# a vector of binomial random var. Given a p(j) prob of success, and n = 1000 voters
# R generare the # of votes for Obama. 
n.states <- nrow(pres08) # number of states
n <- 1000 # number of respondents
sims <- 10000 # number of simulations
## Obama’s electoral votes
Obama.ev <- rep(NA, sims)
for (i in 1:sims) {
  ## samples number of votes for Obama in each state
  draws <- rbinom(n.states, size = n, prob = pres08$p)
  ## sums state’s Electoral College votes if Obama wins the majority
  Obama.ev[i] <- sum(pres08$EV[draws > n / 21])
}
hist(Obama.ev, freq = F, main = "Prediction of election outcome",
    xlab = "Obama's electoral college votes")
abline(v = 364, col = "blue") # actual result

### Law of large numbers
sims <- 1000
## 3 separate simulations for each
x.binom <- rbinom(sims, p = 0.2, size = 10)
## computing sample mean with varying sample size
mean.binom <- cumsum(x.binom) / 1:sims
## default runif() is uniform(0, 1)
x.unif <- runif(sims)
mean.unif <- cumsum(x.unif) / 1:sims
## plot for binomial
plot(1:sims, mean.binom, type = "l", ylim = c(1, 3),
     xlab = "Sample size", ylab = "Sample mean", main = "Binomial(10, 0.2)")
abline(h = 2, lty = "dashed") # expectation
## plot for uniform
plot(1:sims, mean.unif, type = "l", ylim = c(0, 1),
     xlab = "Sample size", ylab = "Sample mean", main = "Uniform(0, 1)")
abline(h = 0.5, lty = "dashed") # expectation

### Central limit theorem
# sims = number of simulations
n.samp <- 1000
z.binom <- z.unif <- rep(NA, sims)
for (i in 1:sims) {
  x <- rbinom(n.samp, p = 0.2, size = 10)
  z.binom[i] <- (mean(x) - 2) / sqrt(1.6 / n.samp)
  x <- runif(n.samp, min = 0, max = 1)
  z.unif[i] <- (mean(x) - 0.5) / sqrt(1 / (12 * n.samp))
}
## histograms; nclass specifies the number of bins
hist(z.binom, freq = FALSE, nclass = 40, xlim = c(-4, 4), ylim = c(0, 0.6),
     xlab = "z-score", main = "Binomial(0.2, 10)")
x <- seq(from = -3, to = 3, by = 0.01)
lines(x, dnorm(x)) # overlay the standard normal PDF
hist(z.unif, freq = FALSE, nclass = 40, xlim = c(-4, 4), ylim = c(0, 0.6),
     xlab = "z-score", main = "Uniform(0, 1)")
lines(x, dnorm(x))
