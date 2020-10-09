library(testthat)
library(ForesToolboxRS)
library(stars)
library(raster)
library(forecast)
context("ForesToolboxRS smootH")

test_smootH <- function(x, interp="na.interp"){

  if (is.vector(x)) {
    x[x==0 | x== -1] <- NA
    x[summary(x)[7] >= (length(x)-1)] <- 100

    # Type of interpolation
    if (interp=="na.interp") {
      x <- na.interp(x)

    } else if (interp=="na.approx") {
      x <- na.approx(x)

    } else if (interp=="na.spline") {
      x <- na.spline(x)

    } else stop("Unsupported interpolation method")

    # We apply Hamunyela Smoothing
    for (j in 2:(length(x)-1)) {
      x[j] <- ifelse(((x[j]-x[j-1]) < -0.01*x[j-1]) & ((x[j]-x[j+1]) < -0.01*x[j+1]),
                     (x[j-1]+x[j+1])/2, x[j])
    }
    np <- x
  } else if (is(x, 'matrix')) {

    for (i in 1:dim(x)[1]) {
      x[i,][x[i,] == 0 | x[i,] == -1]<- NA
      x[i,][summary(x[i,])[7] >= (dim(x)[2]-1)] <- 100

      # Type of interpolation
      if (interp=="na.interp") {
        x[i,] <- na.interp(x[i,])

      } else if (interp=="na.approx") {
        x[i,] <- na.approx(x[i,])

      } else if (interp=="na.spline") {
        x[i,] <- na.spline(x[i,])

      } else stop("Unsupported interpolation method")
    }
    # We apply Hamunyela Smoothing
    for(i in 1:dim(x)[1]){
      for(j in 2:(dim(x)[2]-1)){
        x[i,][j]<-ifelse(((x[i,][j]-x[i,][j-1])< -0.01*x[i,][j-1]) & ((x[i,][j]-x[i,][j+1])< -0.01*x[i,][j+1]),
                         (x[i,][j-1]+x[i,][j+1])/2,x[i,][j])
      }
    }
    np <- x
  } else if (is(x,'RasterStack') | is(x,'RasterBrick')){
    np <- as.matrix(x)
    for (i in 1:dim(np)[1]) {
      np[i,][np[i,] == 0 | np[i,] == -1]<- NA
      np[i,][summary(np[i,])[7] >= (dim(np)[2]-1)] <- 100

      # Type of interpolation
      if (interp=="na.interp") {
        np[i,] <- na.interp(np[i,])

      } else if (interp=="na.approx") {
        np[i,] <- na.approx(np[i,])

      } else if (interp=="na.spline") {
        np[i,] <- na.spline(np[i,])

      } else stop("Unsupported interpolation method")
    }

    # We apply Hamunyela Smoothing
    for(i in 1:dim(np)[1]){
      for(j in 2:(dim(np)[2]-1)){
        np[i,][j]<-ifelse(((np[i,][j]-np[i,][j-1])< -0.01*np[i,][j-1]) & ((np[i,][j]-np[i,][j+1])< -0.01*np[i,][j+1]),
                          (np[i,][j-1]+np[i,][j+1])/2,np[i,][j])
      }
    }
  }
  else {
    stop(class(x), ' class is not supported')
  }
  return(np)
}

test_that("try smoothing", {
  x <- c(80,78,75,76,79,-100,82,76,81,77,76)
  expect_equal(smootH(x),test_smootH(x))
})
