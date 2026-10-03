################################
## R Script 5
## Objective: Merging, Loops,
##            If/Else Statements
################################
rm(list=ls())

setwd("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/PS2")

library(readstata13)
censorship <- read.dta13("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 2/Censorship data.dta")
milexp <- read.dta13("~/Dropbox/My Mac (Apple’s MacBook Air)/Desktop/Purdue/Fall 2026/Intro to Research Methods/Week 2/milexp.dta")

### INTRO TO MERGING

merged.data <- merge(censorship, milexp, by=c("ccode", "year"))
not.merged <- censorship$country[!(censorship$country %in% merged.data$country)]

ccode1 <- censorship$ccode[censorship$country=="Ethiopia"]
ccode2 <- milexp$ccode[milexp$country=="Ethiopia"]

censorship$ccode[censorship$country=="Ethiopia"] <- 530

merged.data <- merge(censorship, milexp, by=c("ccode", "year"))
not.merged <- censorship$country[!(censorship$country %in% merged.data$country)]

head(merged.data)

### IF / ELSE STATEMENTS

artists <- c("Avicii", "Stromae", "Bob Dylan")

if ("Avicii" %in% artists) { # Fill in the blank
    cat("Wake me up") # Fill in blank with: " 'Avicii' has 6 letters ", with 6 calculated from length of word.
} else {
    cat("Avicii is not in artists\n")
}

# cat() is similar to print() but allows more flexibility regarding new lines, separation, overwriting content, etc. 

if ("Tove Lo" %in% artists) { # Fill in the blank
  cat("Talking body") # Fill in blank with: " 'Avicii' has 6 letters ", with 6 calculated from length of word.
} else if ("Stromae" %in% artists) { # Fill in the blank
  cat("Où t'es, papaoutai?")
} else {
  cat("Did you mean Bob Dylan? \n")
}


#Else if is optional, ONLY if you have more than 2 choices. 

# One of my favorite commands: ifelse()

ifelse(artists=="Stromae", T, F)

milexp$cold_war <- ifelse(milexp$year <= 1991, 1, 0)
table(milexp$cold_war)

head(milexp)


### INTRO TO LOOPS

# FOR loops and WHILE loops. FOR loops iterate through all items in an object. 
# WHILE loops operate until a specified condition doesn't apply anymore.

# Normal FOR loop example:
u1 <- rnorm(20, 0, 1)
x <- NULL #WHY?
for (i in 1:length(u1)) {
  x[i] <- u1[i]*u1[i]
  print(x[i])
  print(i)
}

# WHILE loop example:
x <- NULL
z <- 1
while(z<=5) {
  x[z] <- u1[z]*u1[z]
  print(x[z])
  print(z)
  z = z+1 ## This is key: you manually add one every iteration, or it'll run infinitely.
  Sys.sleep(2) # Wait to seconds after every run
}


## Loops are best learned through practice. So let's practice. 

#Exercise 1: Write a for loop that iterates over the numbers 3 to 19 and prints the cube of each number using print().
#            Use the sequence object below in the loop. 
seq <- c(3:19)
for (i in seq) {
  print(i^3)
}


#Exercise 2: In the loop above, stop printing if the cubed result is greater than 2500.
seq <- c(3:19)
for (i in seq) {
  if (i^3 > 2500) {
    break
  }
  print(i^3)
}




#Exercise 3: Using the "rnorm(20, 0, 1)" function (see example above), write a for loop that performs 
#            100 different draws. Stores the mean of each draw into a new object called "means", and the 
#            standard deviation into an object called "stdev".




#Exercise 4: Quick simulation
library(arm)
n <- 1000
sims <- 1000
b1 <- NULL
se.b1 <- NULL
intercept <- 2
b1.true <- 0.75
for (i in 1:sims) {
  x <- rnorm(n, 5, 1)
  e <- rnorm(n, 5, 1)
  y <- intercept + b1.true * x + e
  reg <- lm(y ~ x)
  summary(reg)
  b1[i] <- coef(reg)[2]
  se.b1[i] <- se.coef(reg)[2]
}
mean(b1); b1.true
mean(se.b1)


#Exercise 5: Using the vector below, write a loop that prints "Large" if the number is large and "Small" otherwise, with the threshold at 100. 



#Exercise 6: Print True of False if length of the artist name 
#             is greater than 7
artists <- c("Leonard Cohen", "Drake", "Katy Perry", 
             "Iggy Pop", "Tove Lo", "Bob Dylan")




## Can we extract the first name of the following artists? You may need two nested loops
artists <- c("Leonard Cohen", "Bob Dylan", "Katy Perry", 
             "Iggy Pop", "Leonard Caprio", "Taylor Swift")






















