### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### 
###  R Script 8: Advanced Regression
### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list=ls())

setwd("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 11")

library(arm)
library(readstata13)
library(lmtest)
library(sandwich)
library(plm)

d <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 11/kellyWitko_stateInequality.csv")


### CLUSTERED STANDARD ERRORS 

m1 <- lm(marketgini ~ nonwhite, data = d)
summary(m1)

m1_clustered <- coeftest(m1, vcov = vcovCL, cluster = ~statename) # by state
head(m1_clustered) # No longer significant

# Very important to cluster SEs when we think there is serial correlation in
# our data, usually across different groups


### UNIT FIXED EFFECTS

# 1. Using lm() with dummies
m_ufe <- lm(marketgini ~ nonwhite + factor(statename), data = d)
summary(m_ufe)

m_ufe_clustered <- coeftest(m_ufe, vcov = vcovCL, cluster = ~statename) # by state
head(m_ufe_clustered) # highly significant when using FE

# 2. Using plm() 
plm_ufe <- plm(marketgini ~ nonwhite, model="within", index=c("statename"), data = d)
summary(plm_ufe)

m1coeffs_cl <- coeftest(plm_ufe, vcov.=function(x) vcovHC(x, type="sss", cluster=c("group")))
head(m1coeffs_cl)


### TWO-WAY FIXED EFFECTS (UNIT + TIME)

# 1. Using lm() with dummies
m_twfe <- lm(marketgini ~ nonwhite + factor(statename) + factor(year), data = d)
summary(m_twfe)

m_twfe_clustered <- coeftest(m_twfe, vcov = vcovCL, cluster = ~statename) # by state
head(m_twfe_clustered) # highly significant when using FE

# 2. Using plm() 
plm_twfe <- plm(marketgini ~ nonwhite + factor(year), model="within", index=c("statename"), data = d)
summary(plm_twfe)

m1coeffs_cl <- coeftest(plm_twfe, vcov.=function(x) vcovHC(x, type="sss", cluster=c("group")))
head(m1coeffs_cl)








