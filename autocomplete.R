
library(testthat)

# Beginning with the read function

input.file <- "baby-names.txt"

read.terms <- function(input.file){
  word.data <- read.table(input.file, sep = "\t", header = TRUE, row.names = NULL)
  names(word.data) <- c("weight", "term")
  return(word.data)
}


words <- read.terms("baby-names.txt")
words <- read.terms("pokemon.txt")

head(words$weight)

# A function that sorts by weight
sort_byWeight <- function(input.words){
  sorted.words <- input.words[order(input.words$weight),] 
  return(sorted.words)
}

words.wSort <- sort_byWeight(words)

# A function that sorts by the first r characters of a name
sort_byTerm <- function(input.words, input.r){
  sorted.words <- input.words[order(substr(input.words$term, 1, input.r)),] 
  
  # Include iterators
  #library(iterators)
  #i1 <- iter(1:10)
  #nextElem(i1)
  
  # Include levels
  #number.strings <- unique(sorted.words$words)
  #sorted.words$stringID <- as.numeric(factor(substr(sorted.words$term, 1, input.r)))
  sorted.words$fragment <- substr(sorted.words$term, 1, input.r)
  
  return(sorted.words)
}


# A binary search algorithm (so far from the HW)

# Instead of using binary search, I am using binary upper and lower bound here

# Binary lower bound
binary_lower_bound <- function(query, seq, first, last){
  
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
  if(query < seq[1]){
    warning("Query is outside the range")
    return(c(first, FALSE))
  }
  if(query > seq[n.seq]){
    warning("Query is outside the range")
    return(c(last, FALSE))
  }
  
  # If the first element of the sequence is equal to query, we return a warning and NA 
  # instead of a position because there is no lower bound
  if(query == seq[1]){
    warning("There is no lower bound")
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
  if(first+1==last && !is.na(seq[last]) && query < seq[last] && query > seq[first]){
    return(c(first, TRUE))
  }else if(query > seq[mid]){
    # We update first and last according to where query is and discard the other half
    first <- mid
    # We iterate with the new section (updated first)
    binary_lower_bound(query, seq, first, last)
  }else if(query == seq[mid]){
    # If first and mid are one apart, and mid is equal to query, first is the lower bound
    if(first+1== mid){return(c(first, TRUE))}
    # Otherwise we use this point as the new upper bound (to be able to deal with ties)
    last <- mid
    binary_lower_bound(query, seq, first, last)
  }else if(query < seq[mid]){
    last <- mid
    # We iterate with the new section (updated last)
    binary_lower_bound(query, seq, first, last)
  }
}


# Binary upper bound
binary_upper_bound <- function(query, seq, first, last){
  
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
  if(query < seq[1]){
    warning("Query is outside the range")
    return(c(first, FALSE))
  }
  if(query > seq[n.seq]){
    warning("Query is outside the range")
    return(c(last, FALSE))
  }
  
  # If the first element of the sequence is equal to query, we return a warning and NA 
  # instead of a position because there is no upper bound
  if(query == seq[n.seq]){
    warning("There is no upper bound")
    return(c(NA, FALSE))
  }
  
  # Finding the middle element mid, which must be an integer
  mid <- trunc(first + (last-first)/2)
  
  
  # This part of the code passes new ranges to binary_upper_bound, depending on if query is in the
  # left or right part of the sequence. If it found the upper bound, it returns it.
  
  # This is the case of where the query lies between two points. Here we return
  # last as the upper bound. We need to make sure that last points at an element
  # in the sequence, if it is outside, the code ignores that case
  if(first+1==last && !is.na(seq[last]) && query < seq[last] && query > seq[first]){
    return(c(last, TRUE))
  }else if(query > seq[mid]){
    # We update first and last according to where query is and discard the other half
    first <- mid
    # We iterate with the new section (updated first)
    binary_upper_bound(query, seq, first, last)
  }else if(query == seq[mid]){
    # If last and mid are one apart, and mid is equal to query, last is the upper bound
    if(last-1== mid){return(c(last, TRUE))}
    # Otherwise we use this point as the new lower bound 
    first <- mid
    binary_upper_bound(query, seq, first, last)
  }else if(query < seq[mid]){
    last <- mid
    # We iterate with the new section (updated last)
    binary_upper_bound(query, seq, first, last)
  }
}

test_that("lower and upper bound frame only equal entries", {
  query <- 234
  first <- binary_lower_bound(234, words.wSort[,1], 1, nrow(words))[1]
  last <- binary_upper_bound(234, words.wSort[,1], 1, nrow(words))[1]
  expect_equal(as.double(unique(words.wSort[(first+1):(last-1), 1])), query)
})


input.query <- "Mar"
input.words <- words
k <- 3
  
autocomplete <- function(input.query, input.words, k){
  # Given a user’s query and an integer k, find the suggestion terms by first
  # sorting the list of terms in lexicographic order, then searching for those
  # which begin with the user’s query (using binary search), then sorting the
  # resulting terms in descending order by weight. Return the top k suggestions
  # and their weights.
  r <- nchar(input.query)
  lex.word <- sort_byTerm(input.words, r)
  query.code <- head(lex.word[substring(lex.word$term, 1, r)== input.query, 3], 
                     n=1)
  low <- binary_lower_bound(input.query, lex.word$fragment, 1, nrow(lex.word)+1)
  high <- binary_upper_bound(input.query, lex.word$fragment, 1, nrow(lex.word)+1)
  
  #low <- binary_lower_bound(query.code, lex.word$fragment, 1, nrow(lex.word)+1)
  #high <- binary_upper_bound(query.code, lex.word$fragment, 1, nrow(lex.word)+1)
  
  relevant.word <- lex.word[(low[1]+1):(high[1]-1),]
  
  return(relevant.word[1:k,])
}



autocomplete("Lynn", words, 10)
autocomplete("Michelle", words, 3)
autocomplete("Mich", words, 3)

autocomplete("Pedr", words, 3)
autocomplete("Erin", words, 5)

# It only works for queries of length 4 or 3
