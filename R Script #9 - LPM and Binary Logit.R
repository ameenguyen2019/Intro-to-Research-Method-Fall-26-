### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
###   Code Script 2: LPM (Linear Prob Model) and Binary logit
###   Created by: Joan Timoneda
###   Date: Nov. 12, 2023
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list = ls())

setwd("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 11")

library(DataCombine)
library(readstata13)
library(lmtest)
library(sandwich)
library(boot)
library(plm)

data <- read.dta13("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 11/inequality_democracy.dta")
View(data)

# Run Logit model: inequality on democracy.

colnames(data) # inequality_knutsen1, inequality_pwt1, inequality_houle1, Top1incomeshare
               # Top01incomeshare, Top5incomeshare

table(data$democracy) # Binary data, slightly more autocracy years than democracy years

# Naive LPM model, no controls:
lpm <- lm(democracy ~ inequality_knutsen1, data=data) # "1" at end means "lagged 1 year"
summary(lpm)
lpm_cl <- coeftest(lpm, vcov=plm::vcovHC(lpm, cluster="ccode")) #Cluster SEs
head(lpm_cl)

# With some controls:
lpm_controls <- lm(democracy ~ inequality_knutsen1 + lgdppc1 + growth1 + loilprod1, data=data)
summary(lpm_controls)
lpm_controls_cl <- coeftest(lpm_controls, vcov=plm::vcovHC(lpm_controls, cluster="ccode")) #Cluster SEs
head(lpm_controls_cl)

# Issues with multicolinearity? 
cor(data[,c("inequality_knutsen1", "lgdppc1", "growth1", "loilprod1")], use="complete.obs")
# [-1, 1]: Perfect negative & positive correlation.


# Naive Logit model, no controls:
logit_model <- glm(democracy ~ inequality_knutsen1, family=binomial(logit), data=data)
summary(logit_model)
logit_model_cl <- coeftest(logit_model, vcov=plm::vcovHC(logit_model, cluster="ccode")) #Cluster SEs
head(logit_model_cl)

# With some controls:
logit_model_controls <- glm(democracy ~ inequality_knutsen1 + lgdppc1 + growth1 + loilprod1, family=binomial(logit), data=data)
summary(logit_model_controls)
logit_model_controls_cl <- coeftest(logit_model_controls, vcov=plm::vcovHC(logit_model_controls, cluster="ccode")) #Cluster SEs
head(logit_model_controls_cl)

# So this is essentially telling us that it is more likely that inequality is lower 
# in democracies (1) than dictatorships (0). It doesn't really get closer to a causal claim.


# Let's plot the predicted probabilities from the models to illustrate the finding: 

# Get range of values for your X (inequality):
x <- seq(min(data$inequality_knutsen1, na.rm = T), max(data$inequality_knutsen1, na.rm = T), 0.1)
x

# Let's start with the LPM (with observed value approach, not setting controls at the mean)
preds_lpm <- NULL # Initialize two empty vectors
se_lpm <- NULL #  Initialize two empty vectors
for (i in 1:length(x)) {  # Loop through the values of inequality
  newd <- data.frame(cbind(data[,c("ccode", "year", "lgdppc1", "growth1", "loilprod1")], x[i])) # construct a new dataset
  colnames(newd)[length(newd)] <- "inequality_knutsen1"
  p <- predict(lpm_controls, newdata=newd, se.fit = TRUE) # Predict the outcome
  preds_lpm[i] <- mean(p$fit, na.rm=T) # Calculate the average predicted value
  se_lpm[i] <- mean(p$se.fit, na.rm=T) # Calculate the average prediction standard error
}

ci_upr_lpm <- preds_lpm + (1.96 * se_lpm)
ci_lwr_lpm <- preds_lpm - (1.96 * se_lpm)

plot(preds_lpm, type="l", bty="l", lwd=2, ylim=c(min(ci_lwr_lpm), max(ci_upr_lpm)),
     main="Effect of Inequality on Democracy", 
     xlab="Level of Inequality", ylab="Predicted Probability")
lines(ci_upr_lpm, lty=2) # upper bound
lines(ci_lwr_lpm, lty=2) # lower bound


# Now the main Logit model
preds_logit <- NULL
se_logit <- NULL
for (i in 1:length(x)) {  
  newd <- data.frame(cbind(data[,c("ccode", "year", "lgdppc1", "growth1", "loilprod1")], x[i]))
  colnames(newd)[length(newd)] <- "inequality_knutsen1"
  p <- predict(logit_model_controls, newdata=newd,  type = "response", se.fit = TRUE)
  preds_logit[i] <- mean(p$fit, na.rm=T)
  se_logit[i] <- mean(p$se.fit, na.rm=T)
}

ci_upr_logit <- preds_logit + (1.96 * se_logit)
ci_lwr_logit <- preds_logit - (1.96 * se_logit)

plot(preds_logit, type="l", bty="l", lwd=2, ylim=c(min(ci_lwr_logit), max(ci_upr_logit)),
     main="Effect of Inequality on Democracy", 
     xlab="Level of Inequality", ylab="Predicted Probability")
lines(ci_upr_logit, lty=2)
lines(ci_lwr_logit, lty=2)

# Very similar, just logit has a slight curvature: 
plot(preds_logit,type="l", bty="l", col="red")
lines(preds_lpm) # compare

# So, ss inequality increases, the predicted probability of democracy (1) decreases


# The same predictions but manually with INVERSE LOGIT FUNCTION from the boot package
coefs <- coef(logit_model_controls)
preds <- NULL
for (i in 1:length(x)) {  
  newd <- data.frame(cbind(data[,c("ccode", "year", "lgdppc1", "growth1", "loilprod1")], x[i]))
  p <- inv.logit(coefs[1] + newd[,6]*coefs[2] + newd[,3]*coefs[3] + newd[,4]*coefs[4] + newd[,5]*coefs[5])
  preds[i] <- mean(p, na.rm=T)
}
preds
preds_logit # compare
# The same predictions but FULLY MANUAL with INVERSE LOGIT FUNCTION

# Now doing this fully manually!
preds <- NULL
for (i in 1:length(x)) {  
  newd <- data.frame(cbind(data[,c("ccode", "year", "lgdppc1", "growth1", "loilprod1")], x[i]))
  p <- mean(1 / (1 + exp(-(coefs[1] + newd[,6]*coefs[2] + newd[,3]*coefs[3] + newd[,4]*coefs[4] + newd[,5]*coefs[5]))), 
              na.rm=T) # this is e^(xB) / 1 + e^(xB)
  preds[i] <- mean(p, na.rm=T)
}
preds
preds_logit
# ALL OF THE ABOVE METHODS PRODUCE THE SAME RESULT. PREDICT() IS JUST USING THE INV LOGIT FUNCTION, 
# OR 1 / 1 + e^(-xB) TO PRODUCE THE PREDICTIONS. BUT IT'S EASIER TO CODE AND GETS YOU CIs, TOO.


# Now let's complicate the models a bit to try to model democratization, not just democracy. That is, 
# how does inequality actually impact the likelihood that a country TRANSITIONS to democracy? 

# Lag DV:
data <- data[order(data$ccode, data$year),]
library(slider)
data <- slide(data, "democracy", "year", "ccode", "democracy1", -1)
# alternative to lag DV
library(dplyr)
data <- data %>%
  arrange(ccode, year) %>%
  group_by(ccode) %>%
  mutate(democracy1 = lag(democracy, 1)) %>%
  ungroup()

# What if we add a lag of democracy at t-1 as predictor? (Autorregressive)
m_lagdv <- glm(democracy ~ inequality_knutsen1 + lgdppc1 + growth1 + loilprod1 + democracy1, family=binomial(logit), data=data)
summary(m_lagdv) # Effect disappears

# Dynamic probit - We can interact the effect of inequality with the lag of democracy to get model a "transition"
m_dp_int <- glm(democracy ~ inequality_knutsen1 + democracy1 + inequality_knutsen1*democracy1, family=binomial(probit), data=data)
summary(m_dp_int)

m_dp_int_controls <- glm(democracy ~ inequality_knutsen1 + democracy1 + inequality_knutsen1*democracy1 +
                     lgdppc1*democracy1 + growth1*democracy1 + loilprod1*democracy1, family=binomial(probit), data=data)
summary(m_dp_int_controls)

# Now we have a better, more dynamic model: what are the predictors at democracy_t-1 that predict the state of 
# democracy (0 or 1) at time t0?



## ## ## ## ## ## ## ## 
## What if we use a different measure of inequality?
## ## ## ## ## ## ## ## 


# Naive model, no controls:
m <- glm(democracy ~ inequality_pwt1, family=binomial(logit), data=data)
summary(m)

# With some controls:
m_controls <- glm(democracy ~ inequality_pwt1 + lgdppc1 + growth1 + loilprod1, family=binomial(logit), data=data)
summary(m_controls)

## APPLY THE SAME CODE TO PLOT THE PREDICTED PROBABILITIES

# What if we add a lag of democracy at t-1 as predictor? (Autorregressive)
m_lagdv <- glm(democracy ~ inequality_pwt1 + lgdppc1 + growth1 + loilprod1 + democracy1, family=binomial(logit), data=data)
summary(m_lagdv) # Effect disappears

# Dynamic probit - We can interact the effect of inequality with the lag of democracy to get model a "transition"
m_dp_int <- glm(democracy ~ inequality_pwt1 + democracy1 + inequality_pwt1*democracy1, family=binomial(probit), data=data)
summary(m_dp_int)

m_dp_int_controls <- glm(democracy ~ inequality_pwt1 + democracy1 + inequality_pwt1*democracy1 +
                           lgdppc1 + growth1 + loilprod1, family=binomial(probit), data=data)
summary(m_dp_int_controls)











