### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### 
###  Section 6: Regression; interpreting coefficients; plots
### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

rm(list=ls())
options(scipen = 999) # disables scientific notation

setwd("/Users/joan/Dropbox/0. Purdue/Purdue Fall'22/POL501/Data")

library(arm)
library(readstata13)

data <- read.dta13("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 2/Censorship data.dta")

## Basic OLS
reg1 <- lm(access ~ gdp, data=data) 
summary(reg1) # Interpret

## What's the distribution of the data like?
plot(density(data$gdp))
plot(density(log(data$gdp)))
data$lgdp <- log(data$gdp)

plot(density(data$access))
plot(density(log(data$access)))

# Why log transform?
x <- c(2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
plot(1:10, x, bty="l")
y <- log(x)
plot(1:10, y, bty="l")
# try it: exp(y[1]), exp(y[2]), exp(y[3])....
plot(exp(y[1]))

# Rerun regression
reg2 <- lm(access ~ lgdp, data=data) 
summary(reg2) # Interpret

## TSS, MSS, ESS

attributes(reg1)

reg1[[2]] # Residuals
reg1[[2]]^2 # Squared residuals
rss <- sum(reg1[[2]]^2) #Total sum of squares
rss
rss2 <- sum(resid(reg1)^2) #using function resid()
rss2

y_bar <- mean(reg1[[12]][[1]]) 
y_bar 
mean(data$access) #Same, R just stores it in the object too
ess <- sum((reg1[[5]]-y_bar)^2)

tss <- rss + ess
tss   

tss.alternative <- sum((data$access-y_bar)^2)
tss.alternative

ess/tss # R2
summary(reg1)$r.squared

reg1 <- lm(access ~ lgdp, data=data) # updated regression with lgdp

plot(data$lgdp,data$access)
abline(reg1)

## Anothe example:

d <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 5/states.csv")

reg1 <- lm(Romney2012 ~ ProChoice, data=d) 
summary(reg1)

# Set zero as = to minimum value:
summary(d$ProChoice)
min(d$ProChoice) #Exctract minimum
d$ProChoice_min <- d$ProChoice - min(d$ProChoice) # Do actual centering
summary(d$ProChoice_min) # Check out new range

# What will happen to the coefficients (intercept and b1) now?
reg2 <- lm(Romney2012 ~ ProChoice_min, data=d) 
summary(reg2)
summary(reg1) # For reference

# TSS, MSS, ESS

attributes(reg1)
summary(reg1)

rss <- sum(reg1[[2]]^2) #Residual sum of squares
rss

y_bar <- mean(reg1[[12]][[1]]) 
y_bar 
mean(d$Romney2012)

ess <- sum((reg1[[5]]-y_bar)^2)

tss <- rss + ess
tss   
tss.alternative <- sum((d$Romney2012-y_bar)^2)
tss.alternative

ess/tss # R2
summary(reg1)$r.squared

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### Plotting with plot() and ggplot()
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

d <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/kellyWitko_stateInequality.csv")

summary(d)
colnames(d)
colnames(d)[grepl('gini', colnames(d))] # find variable names by pattern
summary(d$marketgini)

par(mar=c(2,2,2,2))
plot(density(d$marketgini, na.rm=T))
abline(v=summary(d$marketgini)[[2]], col="red") # 2nd position is 25th percentile
abline(v=summary(d$marketgini)[[5]], col="red") # 5th position is 75th percentile
text(0.43,10, "1st quartile", col="red")
text(0.53,10, "3rd quartile", col="red")
text(0.43,1, "25%", col="blue", cex=1.5)
text(0.53,1, "25%", col="blue", cex=1.5)
text(0.48,1, "50%", col="blue", cex=1.5)

plot(density(d$nonwhite, na.rm=T))
table(d$fedminincrease)

### RUN THE MODEL 

m1 <- lm(marketgini ~ nonwhite, data = d)
summary(m1)
attributes(m1)

# With confidence intervals, using base R plot
range <- seq(min(d$nonwhite), max(d$nonwhite), 0.005)
nd <- data.frame(nonwhite=range)
preds <- predict(m1, newdata = nd, interval="confidence", level=0.95)
plot(x=NULL, y=NULL, ylim=c(min(preds[,2]), max(preds[,3])), 
     xlim=c(min(d$nonwhite), max(d$nonwhite)), bty='l', main="Awsome Preds")
lines(range, preds[,1], col="black", lwd=2)
lines(range, preds[,2], col="black", lwd=2, lty=2)
lines(range, preds[,3], col="black", lwd=2, lty=2)

# With GGPLOT
library(ggplot2)

ggplot(d, aes(x=nonwhite, y=marketgini)) + 
  theme_classic() +
  geom_smooth(method=lm, color="black") + # Computes CIs automatically
  labs(title="Awsome Preds", x="Non-White", y="Inequality") +
  theme(plot.title=element_text(size=12, face="bold", hjust=0.5, lineheight=1.2),
        axis.text=element_text(size=11),
        axis.title=element_text(face = "italic"))
  
# This works well with bivariate regression, but will be different in multivariate. 
# You'll most likely need to create a table before ggplot() with all axis, CI, and X data
# See the example below where such a table is created before plotting-


## ANOTHER GGPLOT EXAMPLE FROM A PLOT I BUILT FOR ONE OF MY PAPERS.
##     I used this one because it has a lot of options specified. Play around with them! 
load("load('~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/preds100Kn1Kg.RData')

# Create a dataframe from which to plot (useful if not just a simple regression)
# Data available in class folder so you can run it as is. Email me if you can't find it. 

d <- data.frame(range,pred.mle, pred.lpm, sort(means.y))

# GGPLOT now:
ggplot(aes(x=range, y=pred.mle), data=d) + #Set x and y, and specify dataframe
  geom_line(aes(y=pred.mle), colour="blue", size=1) +
  geom_line(aes(y=pred.lpm), colour="red", size=1) +
  geom_line(aes(y=sort(means.y)), colour="black", size=0.5) +
  geom_hline(yintercept = 0, linetype=2,size=0.5) +
  geom_vline(xintercept = quantile(range,0.95), linetype=3,size=0.5) +
  geom_vline(xintercept = quantile(range,0.05), linetype=3,size=0.5) +
  theme_classic() + labs(title="100K Obs. 1K Groups (100 obs./group)",
                         x="x1", y="Probability of y = 1") +
  xlim(-2,2.5) +  ylim(c(-0.01, max(pred.lpm)+0.015)) + 
  theme(plot.title=element_text(size=12, face="bold", hjust=0.5, lineheight=1.2),
        axis.text=element_text(size=11),
        axis.title=element_text(face = "bold.italic")) +
  annotate("text", x = -1.25, y = 0.04, label = 'atop(bold("True Prob."))', size=3.5, parse=T) +
  annotate("text", x = -1.25, y = 0.035, label = 'atop(bold("LPM"))', size=3.5, color="red", parse=T) +
  annotate("text", x = -1.25, y = 0.03, label = 'atop(bold("MLE"))', size=3.5, color="blue", parse=T)
# Save the plot:
ggsave("Graphs/100Kn-1Kg.pdf")

# To get a more typical ggplot() look, remove option 'theme_classic()' altogether. 


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
###   TABLES
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

d <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/kellyWitko_stateInequality.csv")

m1 <- lm(marketgini ~ nonwhite, data = d)
summary(m1)
m2 <- lm(marketgini ~ pop65, data = d)
summary(m2)

library(stargazer)
stargazer(m1)
?stargazer # latex is default
#Other options:
stargazer(m1, type="html", out="tab.html")
stargazer(m1, type="text", out="tab.txt")
# A bit fancier:
stargazer(m1, column.labels = 'Inequality',
          dep.var.labels.include = FALSE,
          covariate.labels = 'Non-White',
          omit.stat = c("adj.rsq", "f", "ser"),
          align = TRUE, title="Just another table",
          digits = 3, out='tableexample.tex') #can use .html or .txt as well


#Two models (and keeping adding others if you want!):
stargazer(m1, m2, type="html", out="tab.html")










