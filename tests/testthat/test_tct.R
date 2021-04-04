library(testthat)
library(ForesToolboxRS)
library(raster)
data(img_l8)

context("ForesToolboxRS tct")

# Landsat4TM
coefcl4 <- matrix(c(
   0.3037,  0.2793,  0.4743, 0.5585,  0.5082,  0.1863,
  -0.2848, -0.2435, -0.5436, 0.7243,  0.0840, -0.1800,
   0.1509,  0.1973,  0.3279, 0.3406, -0.7112, -0.4572),
   3, 6, byrow = TRUE) # Brightness, Greenness, Wetness

tct_val <- raster::as.matrix(img_l8) %*% t(coefcl4)

# Apply function
sat_tct <- tct(img = img_l8, sat = "Landsat4TM")

# Landsat4TM TCT test
test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[1,]), tct_val[1,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[2,]), tct_val[2,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[3,]), tct_val[3,])
})

# Landsat5TM
coefcl5 <- matrix(c(
   0.2909,  0.2493,  0.4806, 0.5568,  0.4438,  0.1706,
  -0.2728, -0.2174, -0.5508, 0.7221,  0.0733, -0.1648,
   0.1446,  0.1761,  0.3322, 0.3396, -0.6210, -0.4186),
   3, 6, byrow = TRUE) # Brightness, Greenness, Wetness

tct_val <- raster::as.matrix(img_l8) %*% t(coefcl5)

# Apply function
sat_tct <- tct(img = img_l8, sat = "Landsat5TM")

# Landsat5TM TCT test
test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[1,]), tct_val[1,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[2,]), tct_val[2,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[3,]), tct_val[3,])
})

# Landsat7ETM
coefcl7 <- matrix(c(
   0.3561,  0.3972,  0.3904, 0.6966,  0.2286,  0.1596,
  -0.3344, -0.3544, -0.4556, 0.6966, -0.0242, -0.2630,
   0.2626,  0.2141,  0.0926, 0.0656, -0.7629,  0.5388),
   3, 6, byrow = TRUE) # Brightness, Greenness, Wetness

tct_val <- raster::as.matrix(img_l8) %*% t(coefcl7)

# Apply function
sat_tct <- tct(img = img_l8, sat = "Landsat7ETM")

# Landsat7ETM TCT test
test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[1,]), tct_val[1,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[2,]), tct_val[2,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[3,]), tct_val[3,])
})

# Landsat8OLI
coefcl8 <- matrix(c(
   0.3029,  0.2786,  0.4733, 0.5599,  0.5080,  0.1872,
  -0.2941, -0.2430, -0.5424, 0.7276,  0.0713, -0.1608,
   0.1511,  0.1973,  0.3283, 0.3407, -0.7117, -0.4559),
   3, 6, byrow = TRUE) # Brightness, Greenness, Wetness

tct_val <- raster::as.matrix(img_l8) %*% t(coefcl8)

# Apply function
sat_tct <- tct(img = img_l8, sat = "Landsat8OLI")

# Landsat8OLI TCT test
test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[1,]), tct_val[1,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[2,]), tct_val[2,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[3,]), tct_val[3,])
})

# Landsat8OLI-Li2016
coefcl8_li <- matrix(c(
   0.2540,  0.3037,  0.3608,  0.3564, 0.7084,  0.2358,  0.1691,
  -0.2578, -0.3064, -0.3300, -0.4325, 0.6860, -0.0383, -0.2674,
   0.1877,  0.2097,  0.2038,  0.1017, 0.0685, -0.7460, -0.5548),
   3, 7, byrow = TRUE) # Brightness, Greenness, Wetness
img_l8 <- stack(img_l8, img_l8[[1]])
tct_val <- raster::as.matrix(img_l8) %*% t(coefcl8_li)

# Apply function
sat_tct <- tct(img = img_l8, sat = "Landsat8OLI-Li2016")

# Landsat8OLI TCT test
test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[1,]), tct_val[1,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[2,]), tct_val[2,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[3,]), tct_val[3,])
})

# Sentinel2MSI
coefcs2 <- matrix(c(
   0.2381,  0.2569,  0.2934,  0.3020,  0.3580,  0.0896,  0.0780,
  -0.2266, -0.2818, -0.3020, -0.4283,  0.3138, -0.1341, -0.2538,
   0.1825,  0.1763,  0.1615,  0.0486, -0.0755, -0.7701, -0.5293),
   3, 7, byrow = TRUE) # Brightness, Greenness, Wetness
tct_val <- raster::as.matrix(img_l8) %*% t(coefcs2)

# Apply function
sat_tct <- tct(img = img_l8, sat = "Sentinel2MSI")

# Landsat8OLI TCT test
test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[1,]), tct_val[1,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[2,]), tct_val[2,])
})

test_that("try tct", {
  expect_equal(as.vector(getValues(sat_tct)[3,]), tct_val[3,])
})
