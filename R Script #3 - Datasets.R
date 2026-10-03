##############################
## R SCRIPT # 3
## Goals:     Working with 
##            datasets (II).
##############################


rm(list=ls())

# First, set your working directory here:
setwd("/Users/apple/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods")

# Now, load the data file called 'Journal data.csv' using last week's code:
journals <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Journal data.csv")
View(journals)
# Look at the code by clicking on the dataset
# What's odd? class of vars?

journals <- read.csv("Journal data.csv")
head(journals) # also click on it type View(journals) to see it better, if you prefer.
journals <- journals[-1,] #Remove first row, it's useless
journals$Journal <- as.character(journals$Journal) #I have to change the class of the variable, otherwise it behaves oddly
journals <- journals[journals$Journal!="",] # Remove empty rows
journals$Acceptance.Rate <- as.numeric(as.character(journals$Acceptance.Rate)) # We have to coerce a factor for later; don't worry, we'll talk more about this later in the course
journals$Turnaround <- as.numeric(as.character(journals$Turnaround)) # We have to coerce a factor for later; don't worry, we'll talk more about this later in the course
# NA warning is normal here; a couple of observations were characters because they were missing anyway

# ATTRIBUTES AND SUMMARY STATISTICS:
nrow(journals) #num of rows
ncol(journals) #num of columns

#If we want to see just a snippet of the data, like the first few rows:
head(journals) #First six rows
journals[c(1:6),] #Equivalent to head() command

#Variable names
colnames(journals) #Variable names; names() works as well
colnames(journals)[1] #Variable names
names <- colnames(journals) #We can also store the names in a vector
#We can change the name of one column or variable.
#Column 5 has a weird and long name. Let's change it.
colnames(journals)[5] <- "Continent"
colnames(journals)

#Simplest five number summary:
summary(journals$Acceptance.Rate) 

# Some basic plots of the variable:
plot(journals$Acceptance.Rate, journals$Turnaround) 
?plot() # R is confused: which "plot" function do you mean? Must specify package like this:
?base::plot() #Play with options. There's a gazillion. Below is a quick example:
plot(density(journals$Acceptance.Rate), xlab="Acceptance Rate", main="usehwiuh")

boxplot(journals[,c("Acceptance.Rate", "Turnaround")])
boxplot(journals[,c(4,3)])
hist(journals$Acceptance.Rate, breaks = 25)

# Quickly compute basic stats:
mean(journals$Acceptance.Rate, na.rm=T)
median(journals$Acceptance.Rate, na.rm=T)
var(journals$Acceptance.Rate, na.rm=T)
sd(journals$Acceptance.Rate, na.rm=T)


###################################
# Subsetting data using conditions:
###################################

#If you want to use subset() command, investigate on your own or ask me. I don't use it.

# To subset, we will need to understand conditions (logical or boolean operators):
# <, >, <=, >=, ==, !=, &, |; for instance:
pi == 3.16 # False
350 > 250 # True
pi <= 3.1416 # True
2 != 2 # False
25 == 25 & 8 < 7 # False
25 == 25 | 8 < 7 # True
# To subset, we will use consitions like these inside the extract brackets [] 
# after the dataset name instead of row numbers, on the left side of the comma.
# for instance: dataset[dataset$variable.name > 10,]

#Which ones are the outliers in acceptance rate?
summary(journals$Acceptance.Rate)
journals$Journal[journals$Acceptance.Rate>25+1.5*(25-12)]     #outliers = Q3 + 1.5 IQR/ Q1 - 1.5 IQR


# Exercise 1: Create a new dataset with only those observations with
#             turnaround below 60 days. Call it "fast.journals". 
fast.journals <- journals[journals$Turnaround < 60,]
View(fast.journals)


# Exercise 2: Create a new dataset with only those observations with
#             turnaround below 60 days and acceptance rate above 15 percent. 
#             Call it "best.bets". 
best.bets <- journals[journals$Turnaround < 60 & journals$Acceptance.Rate > 15,]
View(best.bets)


# Exercise 3: Extract the turnaround for AJPS. Just print it, don't store it.
journals[4, 4]



# Exercise 4: Keep only three variables: the journal name, acceptance rate, and turnaround. 
#             Override dataset.
journal1 <- journals[ ,c("Journal", "Acceptance.Rate", "Turnaround")]
View(journal1)





