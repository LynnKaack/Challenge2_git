# Challange 2, Autocomplete me
# Lynn Kaack
# A test suite for the binary lower bound and upper bound functions

# Lynn Kaack

library(testthat)

#source("autocomplete.R") # needs to be sourced manually


test_that("it handles a number of equal elements in seq correctly", {
  test.seq <- c(1, 2, 4, 4, 4, 6)
  # If query is equal to those elements
  expect_equal(binary_lower_bound(4, test.seq, 1, length(test.seq)+1), c(2, 1))
  expect_equal(binary_upper_bound(4, test.seq, 1, length(test.seq)+1), c(6, 1))
  # If query is smaller than those elements
  expect_equal(binary_upper_bound(3, test.seq, 1, length(test.seq)+1), c(3, 1))
  # and if it is larger
  expect_equal(binary_lower_bound(5, test.seq, 1, length(test.seq)+1), c(5, 1))
})  


test_that("If query is the largest or smallest element of the sequence we get a warning
          for lower or upper bound", {
  test.seq <- c(1, 2, 4, 4, 4, 5)
  
  # Last point of sequence: For the upper bound we expect a warning
  expect_warning(binary_upper_bound(5, test.seq, 1, length(test.seq)+1))
  # but the lower bound should work as usual
  expect_equal(binary_lower_bound(5, test.seq, 1, length(test.seq)+1), c(5, TRUE))
  
  # First point of sequence: For the lower bound we expect a warning
  expect_warning(binary_lower_bound(1, test.seq, 1, length(test.seq)+1))
  # but the upper bound should work as usual
  expect_equal(binary_upper_bound(1, test.seq, 1, length(test.seq)+1), c(2, TRUE))
})


test_that("if query is outside the range, we get a warning message", {
  test.seq <- c(1, 2, 3)
  expect_warning(binary_lower_bound(4, test.seq, 1, 4))
  expect_warning(binary_upper_bound(4, test.seq, 1, 4))
  expect_warning(binary_lower_bound(0, test.seq, 1, 4))
  expect_warning(binary_upper_bound(0, test.seq, 1, 4))
})


test_that("if first >= last we get an error", {
  expect_error(binary_lower_bound(4, c(2,4,5), 2, 2))
  expect_error(binary_lower_bound(4, c(2,4,5), 3, 2))
  expect_error(binary_upper_bound(4, c(2,4,5), 2, 2))
  expect_error(binary_upper_bound(4, c(2,4,5), 3, 2))
})


test_that("if first is outside the sequence, we get an error", {
  expect_error(binary_lower_bound(3, c(2, 3, 4, 5), 0, 5))
  expect_error(binary_lower_bound(3, c(2, 3, 4, 5), 5, 6))
  expect_error(binary_upper_bound(3, c(2, 3, 4, 5), 0, 5))
  expect_error(binary_upper_bound(3, c(2, 3, 4, 5), 5, 6))
})


test_that("lower and upper bound frame only equal entries", {
  data <- sort(sample(c(364,24,344,345,2,45,234, 23, 5262, 47), size = 1000, replace = TRUE))
  query <- 234
  first <- binary_lower_bound(234, data, 1, length(data))[1]
  last <- binary_upper_bound(234, data, 1, length(data))[1]
  # If query exists
  if(first+1!=last){
    expect_equal(unique(data[(first+1):(last-1)]), query)
  }
})


# Large number of random inputs, numeric and character----

# We generate a large number of random sequences to test how well they match

random.comparison <- function(n.sequence=100, strings = "yes"){
  if(strings == "yes"){
    # Vector with random strings
    random.words <- sort(replicate(paste(sample(letters, size = 10, replace=TRUE), 
                                         collapse=""), n = n.sequence))
  }else{
    # Drawing the random sequence of length=100
    input.seq <- sort(sample(1:40, replace = TRUE, size = n.sequence))
  }

  # Picking the selected query position
  query.position <- sample(1:n.sequence, size = 1)
  input.query <- input.seq[query.position]
  input.first <- 1
  input.last <- n.sequence+1
  
  low <- binary_lower_bound(input.query, input.seq, input.first, input.last)[1]
  high <- binary_upper_bound(input.query, input.seq, input.first, input.last)[1]
  
  # Performing the tests
  test_that("the query is between the low and high bounds", {
    # Query could be the first or last element of the sequence, then one of the bounds should be NA
    if(is.na(low) | is.na(high)){
      expect_true(input.seq[1]==input.seq[query.position] ||
                    input.seq[n.sequence]==input.seq[query.position])
    }else{
      # if it is not the first or last element we expect first and last to frame query
      expect_true(low < query.position & query.position < high)
    }
  })
}

# Running the test for n=300 different random cases
# with strings
replicate(random.comparison(strings = "yes"), n = 300)
# with numbers
replicate(random.comparison(strings = "no"), n = 300)
