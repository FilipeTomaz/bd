# testing package funcitonality

install.packages("testthat")
install.packages("usethis")
library(testthat)
library(usethis)


test_that(".csv data files are available", {
  testthat::expect_equal(list.files(system.file("extdata", package = "fars")),
                         c("accident_2013.csv.bz2",
                           "accident_2014.csv.bz2",
                           "accident_2015.csv.bz2"))
})
