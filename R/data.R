#' Signatures data
#'
#' A dataset containing signatures such as forest (cod1), urban (cod4), agriculture (cod3) and water (cod2).
#'
#' @format A SpatialPointsDataFrame object with 60 features and one variable:
#' \describe{
#'   \item{class}{a value between 1 and 4}
#' }
"endm"

#' A landsat 8 image I
#'
#' A dataset containing a Landsat 8 image with the following bands: blue, green, red, nir, swir1 and swir2
#'
#' @format A RasterBrick object with 6 layers representing optical bands of Landsat 8
"img_l8"

#' A landsat 8 image II
#'
#' A dataset containing a Landsat 8 image with the following bands: blue, green, red, nir, swir1 and swir2
#'
#' @format A RasterBrick object with 6 layers representing optical bands of Landsat 8
"img_optical"


#' A Sentinel-1 image
#'
#' A dataset containing a radar image (Sentinel-1)
#'
#' @format A RasterBrick object with 4 layers representing bands of Sentinel-1
"img_radar"


#' A CLASlite example
#'
#' A dataset containing a photosynthetic vegetation time series obtained from CLASlite.
#'
#' @format A RasterBrick object with 26 layers representing a time-series
"serie_pv"
