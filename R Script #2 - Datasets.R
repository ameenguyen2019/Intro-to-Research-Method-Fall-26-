##############################
## R SCRIPT # 2
## Goals:     Working with 
##            datasets (I).
##############################


###### Load a dataset:

# First things first: make sure the dataset is saved in your working directory, otherwise R won't find it.
# The function you use to read in the data varies by format: .dta (from Stata), .csv, .xlsx, etc.
# Common ones are: read.csv, read.txt, read.table, read.xlsx, read.dta, read.dta13. 
#     * For some of these you need specific packages such as "foreign" and "readstata13".

# Small exercise: install and call the packages 'foreign' and 'readstata13' 

rm(list=ls())

setwd("/Users/joan/Dropbox/0. Purdue/Purdue Fall'21/POL501/Data") # Change to your path
setwd("/Users/apple/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods")

#Read in a stata file:
library(readstata13)
censor <- read.dta13("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 2/Censorship data.dta") 
# ignore warnings here. Warnings aren't errors, so the code still runs. Still, you need to make
# sure that the warning isn't relevant to you or that you have a way to fix it after importing the data.

#Csv File:
journal <- read.csv("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 2/Journal data.csv")
#Same as:
journal <- read.table("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 2/Journal data.csv", header=T, sep=",") #You have to tell it how data is separated.

#View the journal dataset to inspect it visually:
View(journal) #Or click on the name in your environment list (better and faster)

head(journal) # just the first few rows

# What's the class of the censor and journal objects?
class(journal)
class(censor) # both are "data.frame" objects. We can use R tricks specific to these objects to modify them.

# How many observations (rows) does the journal data have?
nrow(journal) 

# How many variables (columns) does the journal data have?
ncol(journal)

# What are the different variables called?
colnames(journal)
names(journal)

# In R, coordinates in the dataset are obtained by adding [,] at the end of dataset object name. The left side
# of the comma is for ROWS, the right side for COLUMNS. Ex: censor[3,2] gets the value for 3rd row, 2nd column.
censor[3,2] #371, the country code for Armenia. 
journal[22,3] # 10, the acceptance rate (%) for the journal "International Organization"

#That first row in the journal data is not useful, let's delete the whole row by leaving the column side of the comma empty. 
journal <- journal[-1,] #Here we OVERWRITE the current object, minus the whole first row.
#Let's get rid of columns 8 and 9 in the censor, they're not useful.
censor <- censor[,-c(8,9)]

# Now we want to create an object with all the country names. We subset the full column and extract it, 
# creating a new, separate object.
country.names <- censor[,1]
country.names # Calling an object by its name prints its contents.

# Let's now do the same using R's unique way of accessing columns in data.frame objects: the $ sign. 
# The $ is used to access variables by name, one at a time.
country.names <- censor$country 
censorship <- censor$tools_score 
# Now we have two vectors with country names and censorship scores.

# Subset on CONDITION: 

# Let's get only those observations that have had some form of political censorship (i.e. greater than 0). 
# We'll create a separate small dataset (a subset) to do this. 
censor_political <- censor[censor$political_score>0,]
nrow(censor_political) # 33 countries have positive political censorship scores. They are:
censor_political$country

# The logic of R is complex, don't worry if it takes a while (or more than that) to sink in. 
# You go from the inside out. Read it like this, starting inside the brackets:
# From all ROWS (see the comma?) that have a political score over 0, EXTRACT those rows from "censor".
# Then place those rows into a NEW object named censor2.
# Let's try again, now using year.

table(censor$year) # Frequency table for 'year' variable

any.name <- censor[censor$year==2009,]

#Subset by columns. Let's keep only the first two columns
any.name2 <- censor[,c(1,2)]
any.name3 <- censor[,c("country", "ccode")]

# Sort a dataset: 
censor <- censor[order(censor$country),] # Sorted by name here

# Creating new variables: 
journal$Acceptance.Rate <- as.numeric(journal$Acceptance.Rate) # Coerce into numeric, somehow original is character
summary(journal$Acceptance.Rate) # Basic statistics
journal$below10 <- ifelse(journal$Acceptance.Rate <= 12, 1, 0) # Dummy variable for journals with acceptance rate below or equal to 1st quartile
table(journal$below10)

journal$Turnaround <- as.numeric(journal$Turnaround) # Why warning? Turns everything that's not a number into NA. 
summary(journal$Turnaround)
journal$fast <- ifelse(journal$Turnaround <= 60, 1, 0) # Dummy variable for fast journals --turnaround below or equal to median
table(journal$fast)

# Table by fast turnaround
table(competitive = journal$below10, fast = journal$fast)



# Saving objects: 
write.csv(journal, "journal_clean.csv") # regular .csv file.
save.dta13(journal, "journal_clean.dta") # Stata file. Still widely used in political science for flexibility.
save(journal, file="journal_clean.RData") # RData object. Small, mainly to be used with R.
library(writexl)
write_xlsx(journal, "journal_clean.xlsx")


################
## EXERCISES: ##
################

# Exercise 1: Subset the censor dataset for observations with CCODE greater than 300.



# Exercise 2: Do the same KEEPING ONLY COLUMNS 'country', 'political_score', 'tools_score', and 'loil' 



# Exercise 3: Sort journal data (journal) by Acceptance.Rate. You'll need to do some cleaning before. 
d <- journal[1:61,]
d <- journal[!is.na(journal$Turnaround),]
d
d <- journal[journal$Journal!="",]   # keep anything different from empty
d


