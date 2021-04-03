library(testthat)
library(ForesToolboxRS)
library(raster)
data(img_l8)

context("ForesToolboxRS sma")

df <- raster::as.matrix(img_l8)

# Endmembers
soil <- c(8980, 8508, 8704, 13636, 16579, 11420)
forest <- c(8207, 7545, 6548, 16463, 9725, 6673)
water <- c(9276, 9570, 10089, 6743, 5220, 5143)
endmembers <- matrix(c(soil, forest, water), 3, 6,
                     byrow = TRUE,
                     dimnames = list(c("soil", "forest", "water"), c("B1", "B2", "B3", "B4", "B5", "B6"))
                     )
# Transpose
M <- t(endmembers)

# Apply spectral mixture analysis
sma <- lapply(1:dim(df)[1], function(i) f<-((solve(t(M)%*%M))%*%t(M))%*%df[i,])

# Fractions
soil <- lapply(1:dim(df)[1], function(i) sma[[i]][1,1])
soil <- as.numeric(soil)
forest <- lapply(1:dim(df)[1], function(i) sma[[i]][2,1])
forest <- as.numeric(forest)
water <- lapply(1:dim(df)[1], function(i) sma[[i]][3,1])
water <- as.numeric(water)

# SMA function
fractions <- sma(img = img_l8, endm = endmembers)

# soil test
test_that("try sma", {
  expect_equal(getValues(fractions$soil), soil)
})

# forest test
test_that("try sma", {
  expect_equal(getValues(fractions$forest), forest)
})

# water test
test_that("try sma", {
  expect_equal(getValues(fractions$water), water)
})
