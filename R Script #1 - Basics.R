##############################
## R SCRIPT # 1
## Goals:     Install R,
##            a few pointers,
##            basic commands.
##############################

# First, install R and RStudio (if you haven't yet). Use the links provided on the syllabus 
# (you can click on them) to download both R and RStudio. RStudio is just a nice shell # 
# for R and it's widely used.

### A few notes to begin with:

# 1. You can run R using native R, RStudio and the command line (terminal on Mac, cmd on Windows)
#      To each their own, but RStudio makes it easier to get comfortable with R, especially at first.

# 2. Console vs. Script. 
#     - Console executes R script.
#     - Script (or 'Source') contains your written code. 
#     - Write EVERYTHING on the .R script first, then execute.
# Other panes on RStudio (environment,plots,files...) Organize these as you wish via global options.

# 3. Windows users: most of this code is written for mac. 99% of it will be the same. Whenever you see
#           references to a shortcut'cmd+', just assume the equivalent is 'ctrl+'. Paths work the same 
#           way for both operating systems. Example: setwd("/Users/Joan/Class")

# Getting started: Symbols, errors, warnings, others:

# 1. In console: '>' means R is ready for new input. You can run your code.  
#                "+" means that R is expecting you to finish some incomplete code. Hit "Esc" if stuck.
# 2. In Script: Comment out a line using "#" (as I am doing here). R ignores these annotations.
#               Comment out full sections by selecting them and hitting Cmd+Shift+C. 
# 3. R is case sensitive. Remember this when creating/calling objects.
# 4. Starting an object with number is not allowed; Periods (".") are allowed inside object names. 
#      i.e. can't name an object '501data', for instance, but 'data501' works. 'data.501' works, too.
# 5. In the console, push the up arrow to get previous commands so you don't have to type them up again.
# 6. You can assign (create) objects using <- or =, but convention is <- (Fierce and rather useless debate.)

#####################
# Alright, let's R. #
#####################

# First, create a script file using "Cmd+Shift+N" and save it. 
#    Note: You should become familiar with keyboard shortcuts, which make writing code much faster.
# In your script, you can use the following as your first line: 
rm(list=ls()) # This command clears your environment removing all lingering objects. 
# Note: Sometimes re-running code from scratch solves errors. You need this line
#       at the top of your file to clear everything first.
# Then you set a working directory and retrieve it using:
setwd("/Users/joan") # Change to your working directory of choice
# R is always linked to a directory on your computer. 
getwd() # prints your current working directory.

# Apart from some basic functions, R works through packages.
# R is open source and programmers are developing packages all the time and nowadays 
# there is a package for pretty much anything.

# To install a package, run line 59 (without the '#'), on the console, not via script:
# install.packages("beepr") (install 1 time)
# NOTE: DO NOT keep any install.packages() command in your script. Otherwise it'll re-install the
# package every time you run the code. Not good. Use once in console only.

beep() 
# ERROR: R can't find the function. Why? You always need to call library(package_name) after installation. ALWAYS.

# Like this:
library(beepr) 

beep() # now it beeps!

# Remember: 
# Often, if a function doesn't work it's because the package hasn't been called. 
# Also, always read the error message. They can be confusing at first but they do help fix the problem!

?beep # This gives you info on using the 'beep()' command from 'beepr'. Inspect the tab that opens up. 
      
# It says that the default sound is '1', ping. Try others.
beep("coin")
beep(8) # Best one by far 

#Now we need to create objects. Objects are the different items in your environment. 
#They can be words, numbers, data frames, combinations of these, etc. They can also be empty or null.
ch <- "iceberg" #character
num <- 2 #numeric 
log <- TRUE #logical

#Objects remain in the environment:
objects()
ls()
#Unless you remove them:
rm(log)
rm(list=ls()) # clears everything in environment

###### Today we'll introduce one type of object: VECTORS

###### Common objects: VECTORS, c() function (from 'concatenate')
ch1 <- c("iceberg", "banana", "tofu")
ch1
num1 <- c(2, 4)
num1 
num2 <- c(1:8)   #create a sequence
num2
num3 <- c(1:8, 2) #Append another number at the end
num3
num4 <- seq(2, 93, 2) #Sequence with different intervals. Function sequence, no need to specify if you follow order.
num4
?seq #get help with a specific function
num4[-length(num4)] #exlude the last number

# To get one specific element or set of elements from a vector, use brackets and indexes:
num4[12]
num4[4:12]

# To delete one object, we use - inside brackets:
num4 <- num4[-1] # we deleted the first element, number 2.
num4

# To get the length of a vector, we use the function length()
length(num4)

# Logical objects, classes and coercion
log1 <- c(TRUE, FALSE, T, F) #just to see the short and long versions. They are equivalent.
log1 

#Check the class of each object
#Classes are important in datasets, as variables behave very differently depending on their class.
class(ch1)
class(num1)
class(num2)
class(log1) #Same as individual classes.
#Let's combine classes, what happens if we do?
comb <- c(2, "night", FALSE) 
comb
class(comb) # all elements are character 
comb2 <- c(2, FALSE) 
comb2 # Now they're both integers

#Append two vectors using same c() function.
comb3 <- c(ch1, num1) 
comb3 #Everything into character. Be careful. 

#There are more classes, such as integer and factor, which I will cover in future sessions
#We can coerce classes if we need to. Here's a brief intro on how to do that (more in future sections as well):

num1
class(num1)
num1 <- as.character(num1)
num1
class(num1) 

# Numbers become words, but words DO NOT become numbers, they go to missing, 
#     which in R is denoted as NA. Same for logical vectors. Check it out:
ch1
class(ch1)
ch1 <- as.numeric(ch1) #See warning message. Not an error, the code executed fine, but R is telling you something is off
ch1
#We can see this much better with the combined vector:
comb
class(comb) #All elements are characters
comb <- as.numeric(comb) 
comb #Now 2 becomes numeric and rest NA.
class(comb) 
#Others include: as.integer(), as.factor(). We will cover this in future sections as well. 
#There are other types of objects, such as lists, matrices, data frames, factors, which I will cover in the future. 
#You're aware of the two most basic types of data you encounter

## BASIC MATH 

# R does all calculations you want

2*2
10/4
sqrt(4)/(8^2+2)
# You get the gist. 
# You can also operate with vectors:
x <- c(1:10)
y <- seq(0.5,5, .5)
x*y
x%*%y # This will be useful way down the road for matrix multiplication
sum(x*y)

# Paths and file organization (below are my folders, try with your own paths):
getwd()
setwd("Dropbox/3 Art. Under Review")
getwd()
setwd("../6 Purdue") # Back one folder, forward one folder
getwd()
setwd("../../../../Downloads") # what happened here?
getwd()

# Please organize your folders and files clearly. 
# In a project, usually you'd have a 'code' folder where you store code files, 
# a 'data' folder where you store your data, etc. You should be able to
# move seamlessly between folders.


################
## EXERCISES: ##
################

# 1. Create a sequence from 3 to 99, in increments of 3. Call the new object 's' and print it. 
s <- seq(3, 99, 3)
s

# 2. Coerce the sequence into a character vector and store it into a new object called 'c'. Print the object
ch <- as.character(s)
ch

# 3. Set the first element of 's' to 0 and print 's'. Then delete the last element of 's' and print it again. (Hard)

s[1] <- 0

s <- s[-length(s)/3] #remove the element at one-third of the vector's length 
s
length(s). # the position was removed

# We're done with the most important basics. Now on to what we do: load, clean and use datasets. 

















