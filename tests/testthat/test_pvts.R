library(testthat)
library(ForesToolboxRS)
library(raster)
context("ForesToolboxRS pvts")

test_pvts_vector <- function(x, startm, endm, threshold = 5) {

    mean.pvts <- mean(x[1:(startm - 1)])

    std.pvts <- sd(x[1:(startm - 1)])

    li <- mean.pvts - threshold * std.pvts

    return(li)
}

test_that("try pvts", {
  x <- c(80, 78, 79, 88, 86, 80, 82, 76, 81, 25, 76)
  cd <- pvts(x, startm = 10, endm = 10, threshold = 5)
  expect_equal(as.vector(cd$Threshold[2]), test_pvts_vector(x, startm = 10, endm = 10, threshold = 5))
})

test_pvts_ts <- function(x, startm, endm, threshold = 5) {

  startm.pvts <- which(time(x) == startm)
  endm.pvts <- which(time(x) == endm)

  mean.pvts <- mean(x[1:(startm.pvts - 1)])

  std.pvts <- sd(x[1:(startm.pvts - 1)])

  li <- mean.pvts - threshold * std.pvts

  return(li)
}

test_that("try pvts", {
  ndfi <- c(
    0.86, 0.93, 0.97, 0.91, 0.95, 0.96, 0.91, 0.88, 0.92, 0.89,
    0.90, 0.89, 0.91, 0.92, 0.89, 0.90, 0.92, 0.84, 0.46, 0.20,
    0.27, 0.22, 0.52, 0.63, 0.61, 0.67, 0.64, 0.86
    )
  ndfi_ts <- ts(ndfi, start = 1990, end = 2017, frequency = 1)
  cd <- pvts(x = ndfi_ts, startm = 2008, endm = 2008, threshold = 5)
  expect_equal(as.vector(cd$Threshold[2]), test_pvts_ts(ndfi_ts,  startm = 2008, endm = 2008, threshold = 5))
})
