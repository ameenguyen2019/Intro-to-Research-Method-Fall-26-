####### Chap 4- Linear regression implementation
face <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 5/PREDICTION/face.csv")
View(face)
### create a scatter plot of competence measure against election outcomes
## two-party vote share for Democrats and Republicans
face$d.share <- face$d.votes / (face$d.votes + face$r.votes)
face$r.share <- face$r.votes / (face$d.votes + face$r.votes)
face$diff.share <- face$d.share - face$r.share
plot(face$d.comp, face$diff.share, pch = 16,
     col = ifelse(face$w.party == "R", "red", "blue"), # red dots used for Rep winners, blue for Dem ones
     xlim = c(0, 1), ylim = c(-1, 1),
     xlab = "Competence scores for Democrats",
     ylab = "Democratic margin in vote share",
     main = "Facial competence and vote share")

### correlation and scatter plots
cor(face$d.comp, face$diff.share)

### run regression
fit <- lm(diff.share ~ d.comp, data = face) # fit the model
summary(fit)
lm(face$diff.share ~ face$d.comp) # method 2
coef(fit) # get estimated coefficients
head(fitted(fit)) # get predicted values
# add a regression line
plot(face$d.comp, face$diff.share, xlim = c(0, 1.05), ylim = c(-1, 1),
     xlab = "Competence scores for Democrats",
     ylab = "Democratic margin in vote share",
     main = "Facial competence and vote share")
abline(fit) # add regression line
abline(v = 0, lty = "dashed")
# obtain residuals
epsilon.hat <- resid(fit) # residuals
sqrt(mean(epsilon.hat^2)) # RMSE
sd(face$d.comp)
sd(face$diff.share)
cor(face$d.comp, face$diff.share)

### Regression toward the mean
pres08 <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 5/PREDICTION/pres08.csv")
View(pres08)
pres12 <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 5/PREDICTION/pres12.csv")
View(pres12)
## merge two data frames
pres <- merge(pres08, pres12, by = "state")
summary(pres)
View(pres)
## change the variable name for illustration
names(pres12)[1] <- "state.abb"
## merging data sets using the variables of different names
pres <- merge(pres08, pres12, by.x = "state", by.y = "state.abb")
summary(pres)
View(pres)
# standardize vote shares across elections 
pres$Obama2008.z <- scale(pres$Obama.x)
pres$Obama2012.z <- scale(pres$Obama.y)
## intercept is estimated as essentially zero
fit1 <- lm(Obama2012.z ~ Obama2008.z, data = pres)
fit1
# plot the predicted regression
plot(pres$Obama2008.z, pres$Obama2012.z, xlim = c(-4, 4), ylim = c(-4, 4),
     xlab = "Obama's standardized vote share in 2008",
     ylab = "Obama's standardized vote share in 2012")
abline(fit1) # draw a regression line
## bottom quartile
mean((pres$Obama2012.z > pres$Obama2008.z)[pres$Obama2008.z <= quantile(pres$Obama2008.z, 0.25)])
## top quartile
mean((pres$Obama2012.z > pres$Obama2008.z)[pres$Obama2008.z >= quantile(pres$Obama2008.z, 0.75)])

### Model fit
florida <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 5/PREDICTION/florida.csv")
## predict Buchanan’s 2000 votes on Perot’s 1996 votes
View(florida)
fit2 <- lm(Buchanan00 ~ Perot96, data = florida)
fit2
## compute TSS (total sum of squares) and SSR (sum of squared residuals)
TSS2 <- sum((florida$Buchanan00 - mean(florida$Buchanan00))^2)
SSR2 <- sum(resid(fit2)^2)
## coefficient of determination
(TSS2 - SSR2) / TSS2
summary(fit2)
# check poor R-square by creating a residual plot
plot(fitted(fit2), resid(fit2), xlim = c(0, 1500), ylim = c(-750, 2500),
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0)
# extract outlier observation
florida$county[resid(fit2) == max(resid(fit2))]
# fit model without outlier (Palm Beach)
florida.pb <- subset(florida, subset = (county != "PalmBeach"))
fit3 <- lm(Buchanan00 ~ Perot96, data = florida.pb)
summary(fit3)
# Create residual plot again without outlier
plot(fitted(fit3), resid(fit3), xlim = c(0, 1500), ylim = c(-750, 2500),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residual plots without Palm Beach")
abline( h = 0)

# plot 2 regression lines in 1 feature
plot(florida$Perot96, florida$Buchanan00, xlab = "Perot's vote in 1996",
     ylab = "Buchanan's vote in 2000")
abline(fit2, lty = "dashed") # regression with Palm Beach
abline(fit3) # regression without Palm Beach 
text(30000, 3250, "Palm Beach")
text(30000, 1500, "regression\n with Palm Beach")
text(30000, 400, "regression\n without Palm Beach")

### Randomized experiments
women <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 5/PREDICTION/women.csv")
View(women)
## proportion of female politicians in reserved GP vs. unreserved GP
mean(women$female[women$reserved == 1])
mean(women$female[women$reserved == 0])
## Test-H: female politicians support female voter's need
# Use dif-in-means
## drinking water facilities
mean(women$water[women$reserved == 1]) - mean(women$water[women$reserved == 0])
## irrigation facilities
mean(women$irrigation[women$reserved == 1]) - mean(women$irrigation[women$reserved == 0]) 
# use regression to analyze randomized experiments, slope beta is the estimated average treatment effect
lm(water ~ reserved, data = women)
lm(irrigation ~ reserved, data = women)

### Multiple regression
social <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 5/PREDICTION/social.csv")
View(social)
# convert to factor var
social$messages <- as.factor(social$messages)
levels(social$messages) # base level is "Civic Duty" = 0
# method 1: fit regressional model using this factor var
fit <- lm(primary2006 ~ messages, data = social)
fit
# method 2: fit regression on indicator var
social$Control <- ifelse(social$messages == "Control", 1, 0)
social$Hawthorne <- ifelse(social$messages == "Hawthorne", 1, 0)
social$Neighbors <- ifelse(social$messages == "Neighbors", 1, 0)
lm(primary2006 ~ Control + Hawthorne + Neighbors, data = social)

### Make predictions for each obs
## create a data frame with unique values of “messages”
unique.mess <- data.frame(messages = unique(social$messages)) #unique () extract unique values & return the order
unique.mess
## make prediction for each observation from this new data frame
predict(fit, newdata = unique.mess)
## linear regression without intercept
fit.noint <- lm(primary2006 ~ -1 + messages, data = social)
fit.noint

### get estimate of average causal effect
## method 1: linear regrssion - estimated average effect of “Neighbors” condition: 
coef(fit)["messagesNeighbors"] - coef(fit)["messagesControl"]
## method 2: difference-in-means
mean(social$primary2006[social$messages == "Neighbors"]) - mean(social$primary2006[social$messages == "Control"])
## adjusted R-squared
adjR2 <- function(fit) {
  resid <- resid(fit) # residuals
  y <- fitted(fit) + resid # outcome
  n <- length(y)
  TSS.adj <- sum((y - mean(y))^2) / (n - 1)
  SSR.adj <- sum(resid^2) / (n - length(coef(fit)))
  R2.adj <- 1 - SSR.adj / TSS.adj
  return(R2.adj)
}
adjR2(fit)
R2(fit) # unadjusted R-squared calculation
R2

#### Heterogenous treatment effects
### examine the dif in ATE of Neighbor message on those voted and didn't
## average treatment effect (ATE) among those who voted in 2004 primary
social.voter <- subset(social, primary2004 == 1) # subset data for those voted
ate.voter <- mean(social.voter$primary2006[social.voter$messages == "Neighbors"]) - mean(social.voter$primary2006[social.voter$messages == "Control"])
ate.voter
## average effect among those who did not vote
social.nonvoter <- subset(social, primary2004 == 0)
ate.nonvoter <- mean(social.nonvoter$primary2006[social.nonvoter$messages == "Neighbors"]) - mean(social.nonvoter$primary2006[social.nonvoter$messages == "Control"])
ate.nonvoter
## difference
ate.voter - ate.nonvoter

### Linear regression w/ interaction term
## subset Neighbors and Control groups
social.neighbor <- subset(social, (messages == "Control") | (messages == "Neighbors"))
## standard way to generate main and interaction effects
fit.int <- lm(primary2006 ~ primary2004 + messages + primary2004:messages,
              data = social.neighbor)
fit.int
## predict primary2006 using 'age'
social.neighbor$age <- 2008 - social.neighbor$yearofbirth
summary(social.neighbor$age)
# compute the estimated difference btw age and neighbors 
fit.age <- lm(primary2006 ~ age * messages, data = social.neighbor)
fit.age
# compute ATE for different ages
## age = 25, 45, 65, 85 in Neighbors group
age.neighbor <- data.frame(age = seq(from = 25, to = 85, by = 20),   # newdata
                           messages = "Neighbors")
## age = 25, 45, 65, 85 in Control group
age.control <- data.frame(age = seq(from = 25, to = 85, by = 20),
                          messages = "Control")
## average treatment effect for age = 25, 45, 65, 85
ate.age <- predict(fit.age, newdata = age.neighbor) - predict(fit.age, newdata = age.control)
ate.age
plot(fitted(fit.age), resid(fit.age), xlim = c(0, 0.6), ylim = c(-0.1, 0.6),
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0)
# solution-use quadratic function of age
fit.age2 <- lm(primary2006 ~ age + I(age^2) + messages + age:messages + I(age^2): messages, data = social.neighbor)
fit.age2
## predicted turnout rate under the Neighbors treatment condition
yT.hat <- predict(fit.age2, newdata = data.frame(age = 25:85, messages = "Neighbors"))
## predicted turnout rate under the Control condition
yC.hat <- predict(fit.age2, newdata = data.frame(age = 25:85, messages = "Control"))
mean(yT.hat - yC.hat)
## plotting the predicted turnout rate under each condition
plot(x = 25:85, y = yT.hat, type = "l", xlim = c(20, 90), ylim = c(0, 0.5),
     xlab = "Age", ylab = "Predicted turnout rate")
lines(x = 25:85, y = yC.hat, lty = "dashed")
text(40, 0.45, "Neighbors condition")
text(45, 0.15, "Control condition")
## plotting the average treatment effect as a function of age
plot(x = 25:85, y = yT.hat - yC.hat, type = "l", xlim = c(20, 90),
     ylim = c(0, 0.1), xlab = "Age",
     ylab = "Estimated average\n treatment effect")

#### Regression Discontinuity design 
mps <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 5/PREDICTION/MPs.csv")
View(mps)
## IV: margin of victory, DV: log net weath
# subset data into 2 parties
mps.labour <- subset(mps, subset = (party == "labour"))
mps.tory <- subset(mps, subset = (party == "tory"))
## two regressions for Labour: negative and positive margin
labour.fit1 <- lm(ln.net ~ margin, data = mps.labour[mps.labour$margin < 0, ])
labour.fit2 <- lm(ln.net ~ margin, data = mps.labour[mps.labour$margin > 0, ])
## two regressions for Tory: negative and positive margin
tory.fit1 <- lm(ln.net ~ margin, data = mps.tory[mps.tory$margin < 0, ])
tory.fit2 <- lm(ln.net ~ margin, data = mps.tory[mps.tory$margin > 0, ])
## Labour: range of predictions
y1l.range <- c(min(mps.labour$margin), 0) # min to 0
y2l.range <- c(0, max(mps.labour$margin)) # 0 to max
## prediction
y1.labour <- predict(labour.fit1, newdata = data.frame(margin = y1l.range))
y2.labour <- predict(labour.fit2, newdata = data.frame(margin = y2l.range))
## Tory: range of predictions
y1t.range <- c(min(mps.tory$margin), 0) # min to 0
y2t.range <- c(0, max(mps.tory$margin)) # 0 to max
## predict outcome
y1.tory <- predict(tory.fit1, newdata = data.frame(margin = y1t.range))
y2.tory <- predict(tory.fit2, newdata = data.frame(margin = y2t.range))

### plot predicted values for each party
## scatter plot with regression lines for Labour
plot(mps.labour$margin, mps.labour$ln.net, main = "Labour",
     xlim = c(-0.5, 0.5), ylim = c(6, 18), xlab = "Margin of victory",
     ylab = "log net wealth at death")
abline(v = 0, lty = "dashed")
## add regression lines for prediction
lines(y1l.range, y1.labour, col = "blue") # min to 0
lines(y2l.range, y2.labour, col = "blue") # 0 to max

## scatter plot with regression lines for Tory
plot(mps.tory$margin, mps.tory$ln.net, main = "Tory",
     xlim = c(-0.5, 0.5), ylim = c(6, 18), xlab = "Margin of Victory",
     ylab = "log net wealth at death")
abline(v = 0, lty = "dashed")
## add regression lines for prediction
lines(y1t.range, y1.tory, col = "red")
lines(y2t.range, y2.tory, col = "red")
## how large is the effect?
## average net wealth for Tory MP
tory.mp <- exp(y2.tory[1]) # take first predicted value for max range, then log scale back to the original wealth scale
tory.mp
## average net wealth for Tory non-MP
tory.nonmp <- exp(y1.tory[2]) # take second predicted value for min range, then log scale back to the original wealth scale
tory.nonmp
## causal effect in pounds
tory.mp - tory.nonmp
# test internal validity using placebo test
## two regressions for Tory: negative and positive margin
tory.fit3 <- lm(margin.pre ~ margin, data = mps.tory[mps.tory$margin < 0, ])
tory.fit4 <- lm(margin.pre ~ margin, data = mps.tory[mps.tory$margin > 0, ])
## the difference between two intercepts is the estimated effect
coef(tory.fit4)[1] - coef(tory.fit3)[1]
