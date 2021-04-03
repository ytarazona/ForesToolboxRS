library(testthat)
library(ForesToolboxRS)
library(raster)
data(img_l8)

context("ForesToolboxRS ndfiSMA")

df <- raster::as.matrix(img_l8)

# Endmembers
endmembers <- matrix(c(
  119.0, 475.0, 169.0, 6250.0, 2399.0, 675.0,
  1514.0, 1597.0, 1421.0, 3053.0, 7707.0, 1975.0,
  1799.0, 2479.0, 3158.0, 5437.0, 7707.0, 6646.0,
  4031.0, 8714.0, 7900.0, 8989.0, 7002.0, 6607.0),
  ncol = 6, nrow = 4, byrow = TRUE) # GV, NPV, Soil, Cloud

rownames(endmembers) <- c("gv", "npv", "Soil", "Cloud")

# Transpose
M <- t(endmembers)

# Apply spectral mixture analysis
sma <- lapply(1:dim(df)[1], function(i) f<-((solve(t(M)%*%M))%*%t(M))%*%df[i,])

# Fractions
gv <- lapply(1:dim(df)[1], function(i) sma[[i]][1,1])
gv <- as.numeric(gv)*100
gv[gv < 0] <- 0
npv <- lapply(1:dim(df)[1], function(i) sma[[i]][2,1])
npv <- as.numeric(npv)*100
npv[npv < 0] <- 0
soil <- lapply(1:dim(df)[1], function(i) sma[[i]][3,1])
soil <- as.numeric(soil)*100
soil[soil < 0] <- 0
cloud <- lapply(1:dim(df)[1], function(i) sma[[i]][4,1])
cloud <- as.numeric(cloud)*100
cloud[cloud < 0] <- 0

# gv + npv + soil + cloud
summed <- gv + npv + soil + cloud
gvs <- (gv / summed) * 100
# npv + soil + cloud
npvSoil <- npv + soil + cloud
# NDFI
ndfi_index <- (gvs - npvSoil) / (gvs + npvSoil)

# SMA function
ndfi <- ndfiSMA(img = img_l8, procesLevel = "SR")

# ndfi test
test_that("try ndfiSMA", {
  expect_equal(as.vector(getValues(ndfi)), as.vector(ndfi_index))
})
