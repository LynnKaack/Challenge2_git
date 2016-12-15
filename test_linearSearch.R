# Challange 2, Autocomplete me
# Lynn Kaack
# Test suite to compare linear search with binary search

library(testthat)

source("linear_search_functions.R")
#source("autocomplete.R") # needs to be sourced manually


# We generate a large number of random sequences to test how well they match

random.comparison <- function(n.sequence=40){
  # Drawing the random sequence and the query
  input.seq <- sort(sample(1:n.sequence, replace = TRUE))
  input.query <- sample(1:n.sequence, replace = TRUE, size = 1)
  input.first <- 1
  input.last <- n.sequence+1
  
  #print(input.query > input.seq[n.sequence])
  
  # Performing the test
  test_that("the results of the linear and of the binary search are identical", {
    expect_equal(linear_lower_bound(input.query, input.seq, input.first, input.last), 
                     binary_lower_bound(input.query, input.seq, input.first, input.last))
    expect_equal(linear_upper_bound(input.query, input.seq, input.first, input.last), 
                 binary_upper_bound(input.query, input.seq, input.first, input.last))
  })
}

# Running the test for n=300 different random cases
replicate(random.comparison(), n = 300)


# In addition, we use some of the edge cases from the test suite from before to 
# compare the functions

test_that("it returns the bounds for a number of equal elements in seq", {
  test.seq <- c(1, 2, 4, 4, 4, 5)
  # If query is equal to those elements
  expect_equal(binary_lower_bound(4, test.seq, 1, length(test.seq)+1), 
               linear_lower_bound(4, test.seq, 1, length(test.seq)+1))
  expect_equal(binary_upper_bound(4, test.seq, 1, length(test.seq)+1), 
               linear_upper_bound(4, test.seq, 1, length(test.seq)+1))
  # If query is smaller than those elements
  expect_equal(binary_lower_bound(3, test.seq, 1, length(test.seq)+1), 
               linear_lower_bound(3, test.seq, 1, length(test.seq)+1))
  expect_equal(binary_upper_bound(3, test.seq, 1, length(test.seq)+1), 
               linear_upper_bound(3, test.seq, 1, length(test.seq)+1))
})  


test_that("If query is the largest element of the sequence get NA for the upper bound", {
  test.seq <- c(1, 2, 4, 4, 4, 5)
  expect_equal(binary_upper_bound(5, test.seq, 1, length(test.seq)+1), 
               linear_upper_bound(5, test.seq, 1, length(test.seq)+1))
})


test_that("if query outside the range, we get an warning message", {
  test.seq <- c(1, 2, 3)
  expect_equal(binary_lower_bound(4, test.seq, 1, 4),
                 linear_lower_bound(4, test.seq, 1, 4))
  expect_equal(binary_lower_bound(0, test.seq, 1, 4),
                 linear_lower_bound(0, test.seq, 1, 4))
  expect_equal(binary_upper_bound(4, test.seq, 1, 4),
               linear_upper_bound(4, test.seq, 1, 4))
  expect_equal(binary_upper_bound(0, test.seq, 1, 4),
               linear_upper_bound(0, test.seq, 1, 4))
})


test_that("if first >= last we get an error", {
  # lower
  # for the binary search
  expect_error(binary_lower_bound(4, c(2,4,5), 2, 2))
  expect_error(binary_lower_bound(4, c(2,4,5), 3, 2))
  # and linear search
  expect_error(linear_lower_bound(4, c(2,4,5), 2, 2))
  expect_error(linear_lower_bound(4, c(2,4,5), 3, 2))
  
  # upper
  # for the binary search
  expect_error(binary_upper_bound(4, c(2,4,5), 2, 2))
  expect_error(binary_upper_bound(4, c(2,4,5), 3, 2))
  # and linear search
  expect_error(linear_upper_bound(4, c(2,4,5), 2, 2))
  expect_error(linear_upper_bound(4, c(2,4,5), 3, 2))
})


test_that("if first is outside the sequence, we get an error", {
  # lower
  # for the binary search
  expect_error(binary_lower_bound(3, c(2, 3, 4, 5), 0, 5))
  expect_error(binary_lower_bound(3, c(2, 3, 4, 5), 5, 6))
  # and linear search
  expect_error(linear_lower_bound(3, c(2, 3, 4, 5), 0, 5))
  expect_error(linear_lower_bound(3, c(2, 3, 4, 5), 5, 6))
  
  # upper
  # for the binary search
  expect_error(binary_upper_bound(3, c(2, 3, 4, 5), 0, 5))
  expect_error(binary_upper_bound(3, c(2, 3, 4, 5), 5, 6))
  # and linear search
  expect_error(linear_upper_bound(3, c(2, 3, 4, 5), 0, 5))
  expect_error(linear_upper_bound(3, c(2, 3, 4, 5), 5, 6))
})

