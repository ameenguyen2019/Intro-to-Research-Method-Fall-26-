###################################
## R Script 7: Working with strings
###################################
rm(list=ls())

## WORKING WITH STRINGS 
cities <- c("New York, NY", "Los Angeles, CA", "Peoria, IL", "Tempe, AZ", 
            "Seattle, WA", "Amarillo, TX")

nchar("Seattle") #Length of string 
nchar(c("Seattle", "nyc", "Tempe")) #Length of each element within a vector of strings

# Paste and cat()
paste("I'm", "in", cities[1]) #concatenate strings
paste("I'm", "in", cities[1], sep="") #defines any separator
paste0("I'm", "in", cities[1]) # Automatic pasting without spaces
paste("Two plus five equals", 2+5) # Also math 

paste("I love", cities)

spotify <- c("Tove Lo", "Stromae", "Bob Dylan")
paste(spotify, "loves", "stats.") #Generates for each name same sentence
paste(spotify, "loves", "stats", collapse=", and ")# Collapse lets you join them together

#To extract a substring
substr("Statistics", 1, 4) #produces 'Stat'
substr("Statistics", 7, 10) #produces 'tics'
substr(spotify, 1, 3) #produces 

#Split string with delimiter: strsplit(string, delimiter)
path <- "/home/mike/data/trials.csv"
strsplit(path, "/") #produces list with separated components in first item

#Replace substrings
s <- "Amy >= Avicii ; Stromae >= Avicii"
sub("Avicii", "Amy", s) #Subs first for second name, only first instance
gsub("Avicii", "Amy", s) #Subs it every instance.
#Generate pairwise combinations of Strings

#Find position of element:
pos = grep('Tempe, AZ', cities)
pos

#Search within strings:
grepl("New", cities)
new.york <- cities[grepl("New", cities)]
cities <- c(cities, "New London, CT", "Newark, NJ") # I'm expanding cities here
new <- cities[grepl("New", cities)]
new # Notice Newark too, any pattern. 

# A little REGEX:
cities[grepl("a", cities, ignore.case = T)] # cities that contain the letter 'a'
cities[grepl("^a", cities, ignore.case = T)] # only ONE city begins with 'a'

cities[grepl("il", cities, ignore.case = T)] # cities that contain the letters 'il'
cities[grepl("il$", cities, ignore.case = T)] # ENDS with 'il', works in this case.
cities[grepl(", IL", cities, ignore.case = T)] # only ONE is state IL, using regular space
cities[grepl(",\\sIL", cities, ignore.case = T)] # using Regex

string <- c("Is it spelled grey or gray?")
string[grepl("gr(a|e)y", string, ignore.case = T)] # using Regex
split <- strsplit(string, c("\\s|\\?"))
split
sapply(split[[1]], function(x) grepl("gray", x))
sapply(split[[1]], function(x) grepl("gr(a|e)y", x))


# Pair all combinations of strings with separator
airport <- c("NY", "LA", "CHI", "HOU")
terminals <- c("T1", "T2", "T3")
m <- outer(airport, terminals, paste, sep="-") #Pairs all with all.
m
m[!upper.tri(m)] #removes repeated permutations, returns character
matrix(m[!upper.tri(m)], 3,3) # same but returns a matrix as in the original


# Exercise 1 (strings): Extract the two-letter abbreviation for each city's state. 
#   Storing those in a new object called 'states'; don't use a loop. 
#   Result should look like "NY", "CA", "IL"... and so on.



#Exercise 2: Add an index from 1 to 6 before each state in 'states'. Replace the original variable.
#         Result looks like: "1. NY", "2. CA..", "3. IL"...



# When is this useful? Example:

x <- rnorm(1000, 0, 1)
y <- rnorm(1000, 0.1*x/2, 1)
cor(x,y)
cities.data <- rep(cities, 125) #just repeating cities to reach 1000 observations
d <- data.frame(cbind(cities.data, x, y))
d$cities.data <- as.character(d$cities.data)

# Create state varible:
d$state <- substr(d[,1], nchar(d$cities.data)-1, nchar(d$cities.data))

#Create indicator based on a pattern:
d$new <- 0
d$new[grepl("New", d$cities.data)] <- 1
