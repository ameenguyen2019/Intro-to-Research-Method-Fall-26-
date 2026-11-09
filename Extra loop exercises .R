################################
## Exercises on Loops
################################

#FROM CLASS, WITH SOLUTIONS:

#Exercise 1 (characters): Extract the state (abbrevation only) for each city, storing those into a new object called 'states'. Write code below.
cities <- c("New York, NY", "Los Angeles, CA", "Peoria, IL", "Tempe, AZ", "Seattle, WA", "Amarillo, TX")
states <- substr(cities, nchar(cities)-1, nchar(cities)) 

#Exercise 2 (loops): Add a number sequentially from 1 to 6 at the end of each state, replacing the original variable
for(i in 1:length(states)) {
  states[i] <- paste(states[i],i, sep="")
}

#NEW EXERCISES:

#Exercise 3: Using the 'year' vector below, create a loop that prints the sentence "This year is ....", where, in the blank
#             space, each year is printed. You should be able to produce 10 sentences in total.  
year <- seq(2007,2016,1)

for(i in 1:length(year)){
  sentences <- paste("This year is ", year[i])
  print(sentences)
}



#Exercise 4: Using the vector below, 
#create a new vector "squared" that stores the square of each number
num <- rnorm(10, 5, 1)
squared <- NULL
for(i in 1:length(num)) {
  squared[i] <- num[i]^2
}
squared




#Exercise 5: Using the rnorm(100, 5, 1) function (random draw of 100 numbers with mean 5 and st.dev. 1), write a for loop that
#             performs 100 different draws and stores the mean of each draw into a new object called "means", and the 
#             standard deviation into an object called "stdev".
x <- rnorm(100, 5, 1)
means <- NULL
for (i in 1:100) {
  sample <- sample(x, 50)
  means[i] <- mean(sample)
}
hist(means, breaks = 10)


