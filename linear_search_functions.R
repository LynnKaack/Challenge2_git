# Challange 2, Autocomplete me
# Lynn Kaack

seq <-  input.seq
query <- 4
first <- 1
last <- 41
linear_lower_bound <- function(query, seq, first, last){
  
  # The edge cases are the same as before
  # Give a custom error if the first and last are equal
  if(first>=last){
    stop("First >= last is not meaningful")
  }
  
  # Error message if first is outside the sequence range, which means it is
  # smaller than 1 or larger than the sequence
  if(first < 1 | first > length(seq)){
    stop("First is outside the sequence range")
  }
  
  # If query is larger than the selected range of the sequence, or smaller than the smallest
  # we return a warning and the position with a FALSE
  if(query < seq[first]){
    warning("Query is outside the range")
    return(c(first, FALSE))
  }
  if(query > seq[last-1]){
    warning("Query is outside the range")
    return(c(last, FALSE))
  }
  
  # The search algorithm is different
  # The linear search:
    for(i in first:(last-1)){
      
      # If exact match, return one lower as the lower bound
      # If the query is the first element, I had the convention to return NA, FALSE
      if(query == seq[i] && i==1){
        return(c(NA, FALSE))
        }else if(query == seq[i]){
          return(c(i-1, TRUE))
          }
      
      # If between points
      if(query < seq[i+1] && query > seq[i]){
        return(c(i, TRUE))
      }
    }
}

linear_upper_bound <- function(query, seq, first, last){
  
  # The edge cases are the same as before
  # Give a custom error if the first and last are equal
  if(first>=last){
    stop("First >= last is not meaningful")
  }
  
  # Error message if first is outside the sequence range, which means it is
  # smaller than 1 or larger than the sequence
  if(first < 1 | first > length(seq)){
    stop("First is outside the sequence range")
  }
  
  # If query is larger than the selected range of the sequence, or smaller than the smallest
  # we return a warning and the position with a FALSE
  if(query < seq[first]){
    warning("Query is outside the range")
    return(c(first, FALSE))
  }
  if(query > seq[last-1]){
    warning("Query is outside the range")
    return(c(last, FALSE))
  }
  
  # For finding the upper bound we can just switch the search
  for(i in (last-1):first){
    
    # If exact match, return one higher as the upper bound
    # If the query is the first element, I had the convention to return NA, FALSE
    if(query == seq[i] && i==(last-1)){
      return(c(NA, FALSE))
    }else if(query == seq[i]){
      return(c(i+1, TRUE))
    }
    
    # If between points
    if(query < seq[i+1] && query > seq[i]){
      return(c(i+1, TRUE))
    }
  }
}

# The test suite that should be fulfilled can be found in 
#test_file("Test_LinearSearch.R")