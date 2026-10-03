###############################
## R script 4 
## Objective: Lists,
##            Frequency, Corr
##            Factors, 
##            Matrices, Tables
###############################

rm(list=ls())

## ADD ISSUES WITH CODE IN HW 1:

# subsetting inside variable
data <- data.frame(names=c("Steve", "Steve", "Ann", "Ann", "Marie", "Marie"), 
                   apples=c(1,2, 20, 30, 200, 300))
data
tapply(data$apples, data$names, mean)
mean(data$apples[data$names=="Steve"])
#Often there's no need to create a separate dummy variable for this, you can just use an existing condition

# subsetting in dataset
ann <- data[data$names=="Ann",]
ann

data$apples[data$names=="Ann" & data$apples==20] <- NA

data$names[!is.na(data$apples)]

# creating new variable
data$apples_100 <- ifelse(data$apples > 100, 1, 0)
data
# try not to create a variable first and then use cbind!

# Others: Typos. Parentheses. Mispelled object names (copy paste good option for this). 
#         Also, try not to print the entire dataset if longer than a few rows/columns. Use head(data) instead.


## LISTS:

# Lists are important for multiple reasons. 
# 1. They accept objects from multiple classes --they don't coerce elements like vectors do.
# 2. Elements in a list can have multiple lengths.
# 3. Flexible and easy to use in for/while loops.
# 4. Access to lapply() --"l" stands for "list", a function similar to loops (but much faster).

lst <- list(2, 4, 0, "moon", 3, FALSE, "ice", 29, c("the", "cardinals", "will", "win", "the", "superbowl"), TRUE)
lst

#Accessing a list; lists contain one or multiple buckets, and
# [] pulls one bucket from the list and [[]] pulls the contents of the bucket

lst[1]
lst[[1]] #See the difference? No [[1]] in the second one, accesses value directly 
lst[[9]]
lst[[9]][2]

#We can have multiple classes in a list
class(lst[[1]])
class(lst[[4]])
class(lst[[length(lst)]]) # What does "length(lst)" do here?

#Let's remove part of a list
lst <- list(2, 4, 0, 3, 4.5, 29, 38) #Create a new list
lst 
lst[lst<7] <- NULL # Delete elements smaller than 7
lst 
lst[1] #Extract "bucket"
lst[[1]] #Extract value
lst[lst == 29] <- NULL #  Here we removed all buckets that are equal to 29
lst
lst[2] <- NA #Add new value to list at second element, assign a missing value. R denotes missing values as NA, while stata does it with a period .
lst 
lst[5] <- NA #Be careful, R will add it and set rest to NULL. If you run something with this the process may pop.
lst 
is.na(lst) # Notice nulls are not NAs.
lst <- lst[-c(3,4)] #Remove nulls
lst
lst[is.na(lst)] <- NULL
lst

lst <- list(1,c(2,3), 8, c(1:10), 3) # Best about lists: elements can have multiple lengths
lst
lst[1]
lst[[2]]
lst[[2]][1]
lst[[2]][2]
lst

lst2 <- list("Arctic", "Monkeys", c("Blink", 142), TRUE, FALSE) # Notice "Blink" "142" was coerced in 3rd element
lst2 # Multiple classes across 'buckets', but not inside each 'bucket'

lst3 <- c(lst, lst2) # bind two lists together
lst3

# Classes within lst3
class(lst3) # Main object is a list
class(lst3[1]) # Class of first bucket is list too, they all are!
class(lst3[[1]]) # Class of the ELEMENTS within bucket is numeric
class(lst3[[6]]) # Class of ELEMENTS in 6th bucket is character
class(lst3[[9]]) # Logical

# we can also just extract one element by position from each 'bucket' in a list
first_elements <- lapply(lst, "[[", 1) # First peak at lapply()
first_elements2 <- lapply(lst2, "[[", 1)

# We can also combine lists into matrices (and then datasets) by cbind() and rbind()

com2 <- cbind(first_elements, first_elements2)
com2 <- data.frame(com2)
com2 
class(com2) # Still, issues with class may come up for individual cells, which haven't been unlisted.

# For that, we can turn lists into data.frames more directly -- useful, for instance, after getting a 
# list object from a loop or lapply(). A couple options to do this:

# Let's create a nested list, which looks a lot like a future dataset:
list <- list()
list <- lapply(list[1:5], function(x) list[x] <- seq(1:10)) # Second peak at lapply(). 'x' in function() is 
                                                            # akin to counter in for loop. 
list

# Option 1: do.call()
new_data <- do.call(rbind.data.frame, list)
class(new_data)
colnames(new_data) <- c(paste0("col",c(1:10))) # rename columns nicely
new_data

# Option 2: ldply()
library(plyr)
new_data <- ldply(list, rbind)
colnames(new_data) <- c(paste0("col",c(1:10))) # column names were already nicer, but still better to rename
new_data

## Correlation and quantiles

setwd("/Users/joan/Dropbox/0. Purdue/Purdue Fall'21/POL501/Data")

library(readstata13)
censor <- read.dta13("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 2/Censorship data.dta")

## Correlation and log transformation

cor(censor$political_score, censor$tools_score)
cor(censor$political_score[censor$political_score>0], censor$tools_score[censor$political_score>0])
#The second condition is still political_score to make comparison of equal length.

cor(censor$access, censor$gdp)

summary(censor$access)
quantile(censor$access)[4]
quantile(censor$access)[[4]]
quantile(censor$access)[[4]] + 1.5*IQR(censor$laccess) # Outlier cut-point

#How are our variables distributed?
plot(density(censor$access))
plot(density(censor$gdp))

# Log transform
censor$lgdp <- log(censor$gdp)
plot(density(censor$lgdp))


#FACTOR VARIABLES, FREQUENCY TABLES and MATRICES

hw <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/hanmerKalkanANES.csv") # Move around directories
table(hw$partyid)
class(hw$partyid)
as.factor(hw$partyid) #But we need labels
# This is, also, how you create a new variable in a dataset using the $ sign
hw$partyid1 <- factor(hw$partyid, 
                     levels=c(0:6),
                     labels=c("Strong Dem.", "Weak Dem.", "Indep. Dem.", "Indep.",
                                "Indep. Rep.", "Weak Rep.", "Strong Rep."))
hw$partyid1

## Factors are categorical variables in R. 
# They can be very tricky. Take this:

hw$partyid1 <- factor(hw$partyid, 
                      levels=c(0:6),
                      labels=c(0:6))
as.integer(hw$partyid1) # Do they match? Why is this happening?
# R assigns the POSITION of the level in the levels sequence, not even its label. What?!
# The way around this is to always coerce into character first. 
as.integer(as.character(hw$partyid1)) # Now it works.

# In R, we generate a frequency table best by first extracting each element from the data, 
# storing it into an object, and then pasting it all together to do a table.
# We know we need: frequency, percent of each category, and cummulative percent.

freq <- table(hw$partyid1)
pct <- prop.table(freq)
cummul <- cumsum(pct)

# Now we have three arrays with information in each. We can transform them into a matrix
# using cbind(), which means that you bind columns together

table <- cbind(freq,pct,cummul)
table
colnames(table) <- c("N", "Pct.", "Cummul.")
table 
class(table)

# Another useful function, rbind() binds rows together. This is how the table would look like, even if we don't want it like that here:

table.r <- rbind(freq,pct,cummul)
table.r

# We can access each element, row, or column as we did with datasets:

table[1,1]
table[,2]
table[4,]

## But be CAREFUL, to rbind() or cbind() ALL objects used need to be of the same length
## If I remove one element from the frequency, 

freq <- freq[-c(2,5)] # removed two data points from freq

## and then try to cbind() with pct and cummul again, R will RECYCLE. This means that it will
## reuse the first value from the set of available values.

table <- cbind(freq,pct,cummul)
table  # Notice 76 and 51 at bottom of freq column --they're repeated from first 2 values!

## Other times, you'll get an error saying that the vectors are not of the same length

## With this in mind, let's try to do a nice table! For this, we need stargazer
library(stargazer)
stargazer(table) # Default is for Latex 
stargazer(table, type="text") # Normal looking table
stargazer(table, out = "table.html") # For .html which then you can open in your browser copy to Word
write.csv(table, "table.csv") 


## ONE LAST IMPORTANT THING, HOW TO SAVE:
save.image("entire_workspace.RData") #This saves the entire workspace, data, objects, open files everything.
# Use load("entire_workspace.RData") to load it after
save(data, file="modified.RData") # Saves a particular object as RData. 
write(tss, file="tss.txt")
write.csv(table, file="table.csv") # No need for .xlsx when you have .csv, but with package xlsx you can save it as excel too. 

