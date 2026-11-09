####### Chap 7-Implementation #######

###  assume that the only confounding factors between
### fast-food restaurants in NJ and those in PA are the fast-food chain to which each
### restaurant belongs, its wage, and the proportion of full-time employment before the
### minimum wage was increased in NJ. Thus, we adjust for these three variables in the
### linear model, where the outcome variable is the proportion of full-time employment
### after the minimum wage was increased in NJ and the treatment variable is whether a
### restaurant is located in NJ
minwage <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 2/CAUSALITY/minwage.csv")
## compute proportion of full-time employment before minimum wage increase
minwage$fullPropBefore <- minwage$fullBefore / 
  (minwage$fullBefore + minwage$partBefore)
## same thing after minimum-wage increase
minwage$fullPropAfter <- minwage$fullAfter /
  (minwage$fullAfter + minwage$partAfter)
## an indicator for NJ: 1 if it’s located in NJ and 0 if in PA
minwage$NJ <- ifelse(minwage$location == "PA", 0, 1)

# regress 3 potential confounding vars
fit.minwage <- lm(fullPropAfter ~ -1 + NJ + fullPropBefore + wageBefore + chain, data = minwage)
fit.minwage
## Call:
## lm(formula = fullPropAfter ~ -1 + NJ + fullPropBefore + wageBefore + chain
### Coefficients:
###   NJ          fullPropBefore       wageBefore  chainburgerking    chainkfc        chainroys  
### 0.05422          0.16879          0.08133         -0.11563         -0.15080         -0.20639  
### chainwendys  
### -0.22013 

# Model 2 includes intercept
fit.minwage1 <- lm(fullPropAfter ~ NJ + fullPropBefore +
                     wageBefore + chain, data = minwage)
fit.minwage1
##
## Call:
## lm(formula = fullPropAfter ~ NJ + fullPropBefore + wageBefore +
## chain, data = minwage)
##
## Coefficients:
## (Intercept) NJ fullPropBefore
## -0.11563 0.05422 0.16879
## wageBefore chainkfc chainroys
## 0.08133 -0.03517 -0.09076
## chainwendys
## -0.10451

# use 2 regression models to predict outcome for 1 obs in data yielding identical predicted value
predict(fit.minwage, newdata = minwage[1, ])
## 1
## 0.2709367
predict(fit.minwage1, newdata = minwage[1, ])
## 1
## 0.2709367

### Inference abt coefficients
# example: randomized experiments, examine the effects of women as policy makers in India
women <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 5/PREDICTION/women.csv")
fit.women <- lm(water ~ reserved, data = women)
summary(fit.women)
# compute CI, using confint()
confint(fit.women) # 95% CI

# compute SE and CI of estimated coefs 
summary(fit.minwage)
# CI for the "NJ" var
confint(fit.minwage)["NJ", ]

## estimate average effect of winning an election on a candidate's wealth in UK
## regression discontinuity
mps <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 11/UNCERTAINTY/MPs.csv")
View(mps)
mps.labour <- mps[mps$party == "labour", ]
mps.tory <- mps[mps$party == "tory", ]
## two regressions for Labour: negative and positive margin
labour.fit1 <- lm(ln.net ~ margin, data = mps.labour[mps.labour$margin < 0, ])
labour.fit2 <- lm(ln.net ~ margin, data = mps.labour[mps.labour$margin > 0, ])
## two regressions for Tory: negative and positive margin
tory.fit1 <- lm(ln.net ~ margin, data = mps.tory[mps.tory$margin < 0, ])
tory.fit2 <- lm(ln.net ~ margin, data = mps.tory[mps.tory$margin > 0, ])
# obtain CI from predict()
## Tory Party: prediction at the threshold
tory.y0 <- predict(tory.fit1, interval = "confidence", newdata = data.frame(margin = 0))
tory.y0
## fit lwr upr
## 1 12.53812 12.11402 12.96221
tory.y1 <- predict(tory.fit2, interval = "confidence", newdata = data.frame(margin = 0))
tory.y1
## fit lwr upr
## 1 13.1878 12.80691 13.56869

## plot 2 regression lines (solid) w/ 95% CI (dashed), using range of predictor x
# first define the two ranges of the electoral margin: min to 0 and 0 to max for data frame of predictions
y1.range <- seq(from = 0, to = min(mps.tory$margin), by = -0.01)
y2.range <- seq(from = 0, to = max(mps.tory$margin), by = 0.01)
# compute predictions for each range with 95% confidence intervals
tory.y0 <- predict(tory.fit1, interval = "confidence", newdata = data.frame(margin = y1.range))
tory.y1 <- predict(tory.fit2, interval = "confidence", newdata = data.frame(margin = y2.range))
## plotting the first regression with losers
plot(y1.range, tory.y0[, "fit"], type = "l", xlim = c(-0.5, 0.5),
     ylim = c(10, 15), xlab = "Margin of victory", ylab = "log net wealth")
abline(v = 0, lty = "dotted")
lines(y1.range, tory.y0[, "lwr"], lty = "dashed") # lower CI
lines(y1.range, tory.y0[, "upr"], lty = "dashed") # upper CI
## plotting the second regression with winners
lines(y2.range, tory.y1[, "fit"], lty = "solid") # point estimates
lines(y2.range, tory.y1[, "lwr"], lty = "dashed") # lower CI
lines(y2.range, tory.y1[, "upr"], lty = "dashed") # upper CI
# compute CI for the difference between these two predicted values
## recompute the predicted value and return standard errors, set se.fit = T
tory.y0 <- predict(tory.fit1, interval = "confidence", se.fit = T, newdata = data.frame(margin = 0))
tory.y0
## $fit
## fit lwr upr
## 1 12.53812 12.11402 12.96221
##
## $se.fit
## [1] 0.2141793
##
## $df
## [1] 119
##
## $residual.scale
## [1] 1.434283
tory.y1 <- predict(tory.fit2, interval = "confidence", se.fit = TRUE,
                   newdata = data.frame(margin = 0))
tory.y1
## s.e. of the intercept is the same as s.e. of the predicted value
summary(tory.fit1)
##
## Call:
## lm(formula = ln.net ~ margin, data = MPs.tory[MPs.tory$margin <
## 0, ])
##
## Residuals:
## Min 1Q Median 3Q Max
## -5.3195 -0.4721 -0.0349 0.6629 3.5798
##
## Coefficients:
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) 12.5381 0.2142 58.540 <2e-16 ***
## margin 1.4911 1.2914 1.155 0.251
## ---
## Signif. codes:
## 0 ’***’ 0.001 ’**’ 0.01 ’*’ 0.05 ’.’ 0.1 ’ ’ 1
##
## Residual standard error: 1.434 on 119 degrees of freedom
## Multiple R-squared: 0.01108, Adjusted R-squared: 0.002769
## F-statistic: 1.333 on 1 and 119 DF, p-value: 0.2506

# compute the standard error of the estimated difference in the average
# log net wealth between MPs and non-MPs at the winning threshold.
## standard error
se.diff <- sqrt(tory.y0$se.fit^2 + tory.y1$se.fit^2)
se.diff # 0.29
## point estimate
diff.est <- tory.y1$fit[1, "fit"] - tory.y0$fit[1, "fit"]
diff.est # 0.65
# CI
ci <- c(diff.est - se.diff * qnorm(0.975), diff.est + se.diff * qnorm(0.975))
ci # [0.0859455, 1.2134268]
## hypothesis test
z.score <- diff.est / se.diff
p.value <- 2 * pnorm(abs(z.score), lower.tail = FALSE) # two-sided p-value
p.value # 0.025
