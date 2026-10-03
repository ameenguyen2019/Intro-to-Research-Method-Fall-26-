###### Chap 3: Implement measurement
afghan <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 3/MEASUREMENT/afghan.csv")
View(afghan)
## summarize variables of interest
summary(afghan)
summary(afghan$age)
summary(afghan$educ.years)
summary(afghan$employed)
summary(afghan$income)
## Analyze if respondents were harmed by ISAF & Taliban
prop.table(table(ISAF = afghan$violent.exp.ISAF, Taliban = afghan$violent.exp.taliban))
## print income data for first 10 respondents
head(afghan$income, n = 10)
## indicate whether respondents’ income is missing
head(is.na(afghan$income), n = 10)
sum(is.na(afghan$income)) # count of missing values
mean(is.na(afghan$income)) # proportion missing
x <- c(1, 2, 3, NA)
mean(x)
mean(x, na.rm = TRUE)
# use table() & keep missing value 
prop.table(table(ISAF = afghan$violent.exp.ISAF, Taliban = afghan$violent.exp.taliban, exclude = NULL))
# na.omit() : listwise deletion
afghan.sub <- na.omit(afghan) # return a subset of data w/ fewer obs
nrow(afghan.sub)
length(na.omit(afghan$income))

### visualize univariate distribution
## a vector of proportions to plot
ISAF.ptable <- prop.table(table(ISAF = afghan$violent.exp.ISAF, exclude = NULL))
ISAF.ptable
## make bar plots by specifying a certain range for y-axis
barplot(ISAF.ptable,
        names.arg = c("No harm", "Harm", "Nonresponse"),
        main = "Civilian victimization by the ISAF",
        xlab = "Response category",
        ylab = "Proportion of the respondents", ylim = c(0, 0.7))
## repeat the same for victimization by the Taliban
Taliban.ptable <- prop.table(table(Taliban = afghan$violent.exp.taliban, exclude = NULL))
barplot(Taliban.ptable,
        names.arg = c("No harm", "Harm", "Nonresponse"),
        main = "Civilian victimization by the Taliban",
        xlab = "Response category",
        ylab = "Proportion of the respondents", ylim = c(0, 0.7))
## make histogram
hist(afghan$age, freq = FALSE, ylim = c(0, 0.04), xlab = "Age", 
     main = "Distribution of respondent's age")
## histogram of education. use “breaks” to choose bins
hist(afghan$educ.years, freq = FALSE,
     breaks = seq(from = -0.5, to = 18.5, by = 1),
     xlab = "Years of education",
     main = "Distribution of respondent's education")
# check summary before adding median line
summary(afghan$educ.years)
## add a text label at (x, y) = (3, 0.5)
text(x = 3, y = 0.5, "median")
## add a vertical line representing median
abline(v = median(afghan$educ.years))
## adding a vertical line representing the median
lines(x = rep(median(afghan$educ.years), 2), y = c(0,0.5))

### boxplot
summary(afghan$age)
boxplot(afghan$age, main = "Distribution of age", ylab = "Age", ylim = c(10, 80))
# boxplot for factor vars
boxplot(educ.years ~ province, data = afghan, main = "Education by province", ylab = "Years of education")
# prop of affirmmative answers to corresponding question for each province
tapply(afghan$violent.exp.taliban, afghan$province, mean, na.rm = TRUE)
tapply(afghan$violent.exp.ISAF, afghan$province, mean, na.rm = TRUE)
# save plots
pdf(file = "educ.pdf", height = 5, width = 5)
boxplot(educ.years ~ province, data = afghan, main = "Education by province", ylab = "Years of education")
dev.off()
### compare multiple plots in a single feature
pdf(file = "hist.pdf", height = 4, width = 8)
## one row with 2 plots with font size 0.8
par(mfrow = c(1, 2), cex = 0.8)
## for simplicity omit the text and lines from the earlier example
hist(afghan$age, freq = FALSE, xlab = "Age", ylim = c(0, 0.04), 
     main = "Distribution of respondent's age")
hist(afghan$educ.years, freq = FALSE, breaks = seq(from = -0.5, to = 18.5, by = 1),
     xlab = "Years of education", xlim = c(0, 20),
     main = "Distribution of respondent's education")
dev.off()

### Randomization
## natural lograrithmic transformation
afghan.village<- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 3/MEASUREMENT/afghan-village.csv")
View(afghan.village)
## box plots for altitude
boxplot(altitude ~ village.surveyed, data = afghan.village,
        ylab = "Altitude (meters)", names = c("Nonsampled", "Sampled"))
## box plots for log population
boxplot(log(population) ~ village.surveyed, data = afghan.village,
        ylab = "log population", names = c("Nonsampled", "Sampled"))

### Nonresponse and other soures of bias
## Compute nonresponse rates
tapply(is.na(afghan$violent.exp.taliban), afghan$province, mean)
tapply(is.na(afghan$violent.exp.ISAF), afghan$province, mean)
# prop of Afghan citizens support ISAF
mean(afghan$list.response[afghan$list.group == "ISAF"]) - mean(afghan$list.response[afghan$list.group == "control"])
# floor effects & ceiling effects
table(response = afghan$list.response, group = afghan$list.group)
