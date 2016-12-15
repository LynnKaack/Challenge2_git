# Challange 2, Autocomplete me
# This is the test suite to ensure that the autocomplete results are correct
# by
autocomplete("Lynn", baby.words, 1)

library(testthat)

#source("autocomplete.R") # needs to be sourced manually

# We work with one data set for simplicity
baby.words <- read.terms("baby-names.txt")

test_that("the weights of the results are in descending order", {
  result <- as.numeric(autocomplete("B", baby.words, 3)$weight)
  expect_equal(result, sort(result, decreasing = TRUE))
})

test_that("if gibberish is inserted, it returns zero and a warning", {
  expect_true(autocomplete("wejbwoibwew", baby.words, 3)==0)
  expect_warning(autocomplete("wejbwoibwew", baby.words, 3)==0)
})

test_that("the function can handle the first and last word in the data frame", {
  # For this we first find the first and last name in the data set
  sorted.baby.words <- sort_byTerm(baby.words, 10)
  first.name <- as.character(sorted.baby.words[1,2])
  last.name <- as.character(tail(sorted.baby.words, n=1)[,2])
  expect_false(nrow(autocomplete(first.name, baby.words, 3))==0)
  expect_false(nrow(autocomplete(last.name, baby.words, 3))==0)
})

test_that("we get a warning if we insert queries outside the range", {
  expect_warning(autocomplete("Aaaaaa", baby.words, 3)==0)
  expect_warning(autocomplete("Zzxy", baby.words, 3)==0)
})
