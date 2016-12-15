# Challange 2, Autocomplete me
# Lynn Kaack
# Performance of the code

library(microbenchmark)

#source("autocomplete.R") # needs to be sourced manually


# Time to load the movies data---------
microbenchmark(read.terms("movies.txt"), times = 1) 
# Result: 
# Around 3.4 seconds
# This is taking a fairly large amount of time. Below we compare it to loading the
# other data frames.


# Time to find matches for different types of input queries------
# I input a list of ten different words to find the average computing time
words <- read.terms("movies.txt")
word.vector <- c("Alice", "Shre", "Ha", "Cars", "I am an", "Emper", "Robot", "The",
                 "A", "Kabhi")
# the mean runtimes for those words are
runtimes.vector <- lapply(X = word.vector, 
                          FUN=function(x){
                            mean(microbenchmark(autocomplete(x, input.data = words, k = 5), times = 100)$time)
                          })
# The summary tells us how much the run times vary between the different words (in nanosec)
summary(unlist(runtimes.vector))
# Results: In average the words required around 2 milliseconds to be found. This however varies
# between 10 milliseconds for "The" (which has a lot of matches), and 0.6 milliseconds for 
# "Emper" (which probably has very few matches). If a query has a lot of matches the
# binary search functions iterate until they find they points bounding those matches. Therefore
# they have more iterations and take longer to run.


# Performance scaling with size of input file (function definition)------
# Function that computes the run times by a varying size of data points in the movie data set
  # Input: A query, which ideally is not contained in the data set to avoid added iterations
          # with more data and a data set (here movies)
  # Output: a data frame with run times as a function of rows in the data set
words <- read.terms("movies.txt")
performance.bySize <- function(input.query, input.data=words, input.function.name){
  n.data <- nrow(input.data)
  r <- nchar(input.query)
  
  # Initializing
  comp.times <- data.frame(matrix(nrow = 20, ncol = 2))
  names(comp.times) <- c("data.size", "time")
  
  # Looping over different sizes of subsamples of the data set
  for(i in 1:20){
    # I use a random subset of the wiktionary data to test different input sizes
      sampled.rows <- sort(sample(1:n.data, replace = FALSE, size = i*10000))
      words.subset <- input.data[sampled.rows, ]
    
    # Storing the size of the data set in the first column
    comp.times[i, 1] <- nrow(words.subset)
    if(i*10000!=comp.times[i, 1]){stop(paste("Problem with sampling in iteration", i))}
    # Storing the mean run time in the second column for the different functions of interest
    if(input.function.name == "autocomplete"){
      comp.times[i, 2] <- mean(microbenchmark(autocomplete(input.query, words.subset, 5), 
                                              times=100L)$time)
      }else if(input.function.name == "binary_lower_bound"){
        comp.times[i, 2] <- mean(microbenchmark(binary_lower_bound(input.query, 
                                                                   words.subset$term, 1, i*10000+1, 
                                                                   accessor = function(x){substr(x, 1, r)}), 
                                                times=100L)$time)
        }else if(input.function.name == "binary_upper_bound"){
          comp.times[i, 2] <- mean(microbenchmark(binary_upper_bound(input.query, 
                                                                     words.subset$term, 1, i*10000+1,
                                                                     accessor = function(x){substr(x, 1, r)}), 
                                                  times=100L)$time)
          }else if(input.function.name == "sort_byWeight"){
            comp.times[i, 2] <- mean(microbenchmark(sort_byWeight(words.subset[1:(i*20),]), times=100L)$time)
            comp.times[i, 1] <- i*20
            }
  }
  return(comp.times)
}


# Performance of autocomplete-----
run.times <- performance.bySize(input.query = "Rwnbw", input.function.name = "autocomplete")
plot(run.times, xlab = "# rows in data", ylab = "time in nanosec", 
     main = "autocomplete")
# Since the binary search functions function are expected to go with O(log(n)), we expect 
# to see something similar for autocomplete, unless there is another operation in automplete
# that dominates the timing. The plot here shows approximately a log function,
# even though there is considerable variability.


# Run time of the read function------

# Initializing the read.terms run time frame
comp.times.read <- data.frame(matrix(nrow = 3, ncol = 2))
names(comp.times.read) <- c("data.size", "time")

# computing the run times (this can be written more compact but I felt it was not making things easier)
comp.times.read[1, 2] <- mean(microbenchmark(read.terms("wiktionary.txt"))$time)
comp.times.read[1, 1] <- nrow(read.terms("wiktionary.txt"))
comp.times.read[2, 2] <- mean(microbenchmark(read.terms("pokemon.txt"))$time)
comp.times.read[2, 1] <- nrow(read.terms("pokemon.txt"))
comp.times.read[3, 2] <- mean(microbenchmark(read.terms("baby-names.txt"))$time)
comp.times.read[3, 1] <- nrow(read.terms("baby-names.txt"))
comp.times.read[4, 2] <- mean(microbenchmark(read.terms("movies.txt"), times = 1)$time)
comp.times.read[4, 1] <- nrow(read.terms("movies.txt"))

plot(comp.times.read, col="red", xlab = "# rows in data", ylab = "time in nanosec",
     main = "read.terms")
# Results: The run times scale approximately linear. Loading and sorting the movie data base
# takes a considerable amount of time. If the data base is only loaded once, this is feasible.


# Run time of specific functions in autocomplete-----

# We look at the performance over time for the binary search functions
# Performance binary_lower_bound
run.times <- performance.bySize(input.query = "Rwnbw", input.function.name = "binary_lower_bound")
plot(run.times, xlab = "# rows in data", ylab = "time in nanosec", 
     main = "binary_lower_bound")
run.times <- performance.bySize(input.query = "Rwnbw", input.function.name = "binary_upper_bound")
plot(run.times, xlab = "# rows in data", ylab = "time in nanosec", 
     main = "binary_upper_bound")
# Result: The binary search function take up each about half of the run time of autocomplete.
# This is expected. If we would perform a linear search, autocomplete would take much longer.
# Both function are expected to go with O(log(n)). The plots show approximately a log function,
# even though there is considerable variability.


# The function to sort the data by weight
run.times <- performance.bySize(input.query = "Rwnbw", input.function.name = "sort_byWeight")
plot(run.times, xlab = "# rows to sort", ylab = "time in nanosec", 
     main = "sort_byWeight")
# Result: The sort function does not take much time if there are not many words that start
# with the query. For movies for example, there are many that start with "the", so here we
# might have instances where this sorting becomes relevant. It looks like it is increasing 
# with n but it is difficult to say what the functional form is.

