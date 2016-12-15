# Challenge 2: Autocomplete me
# Lynn Kaack


# The read.terms function------

# I needed to make sure that names with spaces and special characters are accepted
read.terms <- function(input.file){
  word.data <- read.table(input.file, sep = "\t", header = TRUE, row.names = NULL, 
                          strip.white=TRUE, quote = NULL, stringsAsFactors=F)
  names(word.data) <- c("weight", "term")
  # We also sort the data by term
  sorted.words <- word.data[order(word.data$term),] 
  sorted.words$weight <- as.numeric(sorted.words$weight)
  return(sorted.words)
}


# Other functions used by autocomplete-----

# A function that sorts by weight
  # Input: A a word data frame
  # Output: The same data frame sorted by the weight column
sort_byWeight <- function(input.words){
  sorted.words <- input.words[order(input.words$weight, decreasing = TRUE),] 
  return(sorted.words)
}


# A binary search algorithm 
# I am using binary upper and lower bound here, very similar to the homework binary search. 
# Just like in the homework, I defined these as the values smaller and larger than the query.

# Binary lower bound
  # Output: This function returns the point in the sequence that is one lower than the query.
  #         For query being equal to the first element in the sequence, it returns NA, which 
  #         can be used in the autocomplete function.
  # We use an accessor function for string inputs to create a substring of a sequence element
  # with the length of the query. For numeric inputs, this is just the elemt of seq.
binary_lower_bound <- function(query, seq, first, last, accessor=function(x) { x }){
  
  # To allow the accessor function to ignore capitalization, we also need to expose the query to it
  query <- accessor(query)
  
  # Give a custom error if the first and last are equal
  if(first>=last){
    stop("First >= last is not meaningful")
  }
  
  # Error message if first is outside the sequence range, which means it is
  # smaller than 1 or larger than the sequence
  n.seq <- length(seq) # Length of the sequence
  if(first < 1 | first > n.seq){
    stop("First is outside the sequence range")
  }
  
  # If query is larger than the selected range of the sequence, or smaller than the smallest
  # we return a warning and the position with a FALSE
  if(query < accessor(seq[1])){
    warning("Query is outside the range")
    return(c(first, FALSE))
  }
  if(query > accessor(seq[n.seq])){
    warning("Query is outside the range")
    return(c(last, FALSE))
  }
  
  # If the first element of the sequence is equal to query, we return a warning and NA 
  # instead of a position because there is no lower bound
  if(query == accessor(seq[1])){
    warning("Initial word fragment found")
    return(c(NA, FALSE))
  }
  
  # Finding the middle element mid, which must be an integer
  mid <- trunc(first + (last-first)/2)
  
  # This part of the code passes new ranges to binary_lower_bound, depending on if query is in the
  # left or right part of the sequence. If it found the lower bound, it returns it.
  
  # This is the case of where the query lies between two points. Here we return first as
  # the lower bound
  # We need to make sure that last points at an element in the sequence, if it is outside,
  # the code ignores that case 
  if(first+1==last && !is.na(accessor(seq[last])) && query < accessor(seq[last]) 
     && query > accessor(seq[first])){
    return(c(first, TRUE))
  }else if(query > accessor(seq[mid])){
    # We update first and last according to where query is and discard the other half
    first <- mid
    # We iterate with the new section (updated first)
    binary_lower_bound(query, seq, first, last, accessor)
  }else if(query == accessor(seq[mid])){
    # If first and mid are one apart, and mid is equal to query, first is the lower bound
    if(first+1== mid){return(c(first, TRUE))}
    # Otherwise we use this point as the new upper bound (to be able to deal with ties)
    last <- mid+1
    binary_lower_bound(query, seq, first, last, accessor)
  }else if(query < accessor(seq[mid])){
    last <- mid
    # We iterate with the new section (updated last)
    binary_lower_bound(query, seq, first, last, accessor)
  }
}

# Binary upper bound
# Output: This function returns the point in the sequence that is one higher than the query.
#         For query being equal to the last element in the sequence, it returns NA, which 
#         can be used in the autocomplete function.
binary_upper_bound <- function(query, seq, first, last, accessor=function(x) { x }){
  
  # To allow the accessor function to ignore capitalization, we also need to expose the query to it
  query <- accessor(query)
  
  # Give a custom error if the first and last are equal
  if(first>=last){
    stop("First >= last is not meaningful")
  }
  
  # Error message if first is outside the sequence range, which means it is
  # smaller than 1 or larger than the sequence
  n.seq <- length(seq) # Length of the sequence
  if(first < 1 | first > n.seq){
    stop("First is outside the sequence range")
  }
  
  # If query is larger than the selected range of the sequence, or smaller than the smallest
  # we return a warning and the position with a FALSE
  if(query < accessor(seq[1])){
    warning("Query is outside the range")
    return(c(first, FALSE))
  }
  if(query > accessor(seq[n.seq])){
    warning("Query is outside the range")
    return(c(last, FALSE))
  }
  
  # If the first element of the sequence is equal to query, we return a warning and NA 
  # instead of a position because there is no upper bound
  if(query == accessor(seq[n.seq])){
    warning("Last word fragment found")
    return(c(NA, FALSE))
  }
  
  # Finding the middle element mid, which must be an integer
  mid <- trunc(first + (last-first)/2)
  
  
  # This part of the code passes new ranges to binary_upper_bound, depending on if query is in the
  # left or right part of the sequence. If it found the upper bound, it returns it.
  
  # This is the case of where the query lies between two points. Here we return
  # last as the upper bound. We need to make sure that last points at an element
  # in the sequence, if it is outside, the code ignores that case
  if(first+1==last && !is.na(seq[last]) && query < accessor(seq[last]) 
     && query > accessor(seq[first])){
    return(c(last, TRUE))
  }else if(query > accessor(seq[mid])){
    # We update first and last according to where query is and discard the other half
    first <- mid
    # We iterate with the new section (updated first)
    binary_upper_bound(query, seq, first, last, accessor)
  }else if(query == accessor(seq[mid])){
    # If last and mid are one apart, and mid is equal to query, last is the upper bound
    if(last-1== mid){return(c(last, TRUE))}
    # Otherwise we use this point as the new lower bound 
    first <- mid
    binary_upper_bound(query, seq, first, last, accessor)
  }else if(query < accessor(seq[mid])){
    last <- mid
    # We iterate with the new section (updated last)
    binary_upper_bound(query, seq, first, last, accessor)
  }
}


# This is the main autocomplete function. It allows to find terms independent of 
# capitalization
autocomplete <- function(input.query, input.data, k){
  r <- nchar(input.query)
  n <- nrow(input.data)

  low <- binary_lower_bound(input.query, input.data$term, 1, n+1, 
                            accessor = function(x){tolower(substr(x, 1, r))})[1]
  high <- binary_upper_bound(input.query, input.data$term, 1, n+1,
                             accessor = function(x){tolower(substr(x, 1, r))})[1]
  # I need to adjust those for the two boundary cases, where we found the first and
  # last word fragment. The binary search functions return NA in this case
  if(is.na(low)){low <- 0}
  if(is.na(high)){high <- n+1}
  

  # If not found we return a warning
  if(low +1 == high){
    warning("There are no word matches.")
    return(0)}
  
  relevant.word <- sort_byWeight(input.data[(low+1):(high-1),])
  
  # Returning the top k suggestions
  return(relevant.word[1:k,])
}



# From the command line

command.input = commandArgs(trailingOnly=TRUE)

file.name <- command.input[1]
query <- command.input[2]
k <- command.input[3]

words <- read.terms(file.name)
autocomplete(query, words, k)

