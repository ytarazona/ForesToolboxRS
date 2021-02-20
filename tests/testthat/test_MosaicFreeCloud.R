# library(testthat)
# library(ForesToolboxRS)
# library(stars)
# library(raster)
# context("ForesToolboxRS MosaicFreeCloud")
# rasterio <- list(nXOff = 50, nYOff = 50)
# rasterio_4D <- list(nXOff = 50, nYOff = 50,bands=c(4,3,2))
# time <- as.Date(c("2016-07-30","2016-08-15","2016-09-16"))
#
# test_that("MosaicFreeCloud3D-character", {
#   img <- system.file("simple_mosaic", package="ForesToolboxRS") %>%
#     list.files("\\.tif$",full.names = TRUE)
#   free_img <- MosaicFreeCloud(img, time, RasterIO=rasterio)
#   expect_equal(mean(getValues(free_img),na.rm=TRUE),178.6082,tolerance=0.0001)
# })
#
# test_that("MosaicFreeCloud3D-RasterStack", {
#   img <- system.file("simple_mosaic", package="ForesToolboxRS") %>%
#     list.files("\\.tif$",full.names = TRUE) %>%
#     stack()
#   free_img <- MosaicFreeCloud(img, time)
#   expect_equal(mean(getValues(free_img),na.rm=TRUE), 192.016, tolerance=0.0001)
# })
#
# test_that("MosaicFreeCloud3D-RasterBrick", {
#   img <- system.file("simple_mosaic", package="ForesToolboxRS") %>%
#     list.files("\\.tif$",full.names = TRUE) %>%
#     stack() %>%
#     brick()
#   free_img <- MosaicFreeCloud(img, time, RasterIO=rasterio)
#   expect_equal(mean(getValues(free_img),na.rm=TRUE),192.016,tolerance=0.0001)
# })
#
# test_that("MosaicFreeCloud3D-stars", {
#   img <- system.file("simple_mosaic", package="ForesToolboxRS") %>%
#     list.files("\\.tif$",full.names = TRUE) %>%
#     read_stars(RasterIO=rasterio)
#   free_img <- MosaicFreeCloud(img, time)
#   expect_equal(mean(getValues(free_img),na.rm=TRUE), 178.6082,tolerance=0.0001)
# })
#
#
# test_that("MosaicFreeCloud4D-stars", {
#   img <- system.file("mosaic", package="ForesToolboxRS") %>%
#     list.files("\\.tif$",full.names = TRUE) %>%
#     read_stars(RasterIO=rasterio_4D)
#   free_img <- MosaicFreeCloud(img, time)
#   expect_equal(mean(getValues(free_img),na.rm=TRUE), 285.6287,tolerance=0.0001)
# })
#
# test_that("MosaicFreeCloud4D-character", {
#   img <- system.file("mosaic", package="ForesToolboxRS") %>%
#     list.files("\\.tif$",full.names = TRUE)
#   free_img <- MosaicFreeCloud(img, time,RasterIO=rasterio_4D)
#   expect_equal(mean(getValues(free_img),na.rm=TRUE), 285.6287,tolerance=0.0001)
# })
