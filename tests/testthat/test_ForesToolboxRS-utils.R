library(raster)
context("ForesToolboxRS::utils")

test_that("utils-rasters", {
  f <- system.file("external/test.grd", package="raster")
  r <- raster(f)
  s <- ftb_whatkinditis(r)
  expect_is(s, "stars")
})

test_that("utils-stars", {
  f <- system.file("external/test.grd", package="raster")
  f <- raster(f)
  s <- st_as_stars(f)
  ss <- ftb_whatkinditis(s)
  expect_equal(ss, s)
})

test_that("utils-character", {
  tif = system.file("tif/L7_ETMs.tif", package = "stars")
  s <- ftb_whatkinditis(tif)
  expect_is(s, "stars")
})

test_that("utils-error", {
  tif = 1
  expect_error(ForesToolboxRS:::ftb_whatkinditis(tif))
})

test_that("nd-stars", {
  f <- system.file("external/test.grd", package="raster")
  f <- stack(f,f,f)
  mess <- is_nD(f)
  expect_equal('3D', mess)
})
