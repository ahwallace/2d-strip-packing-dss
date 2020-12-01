## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(testthat)

## -----------------------------------------------------------------------------
skip_if_Tuesday <- function() {
  if (as.POSIXlt(Sys.Date())$wday == 2) {
    skip("Not run on Tuesday")
  }
}

## -----------------------------------------------------------------------------
test_that("skips work", {
  # Test that a skip happens
  expect_error(skip("Hi"), class = "skip")  
  
  # Test that a skip doesn't happen
  expect_error("Hi", NA, class = "skip")  
})

