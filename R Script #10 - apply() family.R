##############################
## R SCRIPT # 5
## Goals:   apply() family
##############################

rm(list=ls())

setwd("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 12")

#Read in a stata file:
library(readstata13)
censor <- read.dta13("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/PS Methodology/Week 2/Censorship data.dta") 

# There are a few VERY useful functions within the apply() family.
# Key advantage: they're faster and more elegant than loops.
# You probably should be using these fairly often. 

# 1. apply() - works for matrices/dataframes.

apply(censor[,1:3], 2, is.na) 
# How many NA values are there in the first 2 columns?
# Note that "2" in the second position refers to columns. 1 would refer to rows. 
apply(censor[,4:7], 2, mean) 
apply(censor[1:4,4:7], 1, mean) # for the first 4 rows, average of all 4 scores per country

# 2. lapply() - works for lists and returns lists

scores <- list(censor$ccode, censor$political_score, censor$tools_score)
scores # list with country codes and two types of scores
length(scores) # 3
lapply(scores, length) # all are 74 in length
lapply(scores, "[[", 3) # extract all THIRD positions from each of the sublists.

lapply(scores, function(x) x^2) # or we can pass a function. In this case, a
# useless squaring of all values that only helps illustrate the point

# lapply can also be applied to dataframes. It's very useful to learn the exact 
# class of a set of columns: 
lapply(censor[,1:10], class)

# 3. sapply() - works for lists but returns vectors or matrices, which often can be easier to work with

sapply(scores, length) # returns vector
sapply(scores, "[[", 3) # returns vector
sapply(scores, function(x) x^2) # returns matrix

# Notice how all the above functions are much more efficient than using loops. 

# There are also tapply() and vapply(), which you should look up on your own. 

# These functions can also be nested, as in:

lapply(lapply(scores, function(x) x^2), mean, na.rm=T)













