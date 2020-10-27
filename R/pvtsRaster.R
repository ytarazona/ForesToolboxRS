#' Change detection using the PVts-\eqn{\beta} approach (raster version)
#'
#' This algorithm will allow to detect disturbances in the forests using
#' all the available Landsat set. In fact, it can also be run with sensors
#' such as MODIS.
#'
#' @author Yonatan Tarazona Coronel
#'
#' @section References:
#' Tarazona, Y., Mantas, V.M., Pereira, A.J.S.C. (2018). Improving tropical
#' deforestation detection through using photosynthetic vegetation time
#' series (PVts-\eqn{\beta}). Ecological Indicators, 94, 367 379.
#'
#' @section Note:
#' In order to optimise the detections, it is advisable to make a smoothing before
#' detecting changes. So the matrix or the stack must go through a smoothing first.
#' The smoothing will allow to eliminate outliers that were not eliminated during the masking.
#' See \link[ForesToolboxRS]{smootH} for more details. In addition, in case the input is a
#' matrix, the first dimension must be rows*columns of the image, and the second dimension the
#' number of images.
#'
#' @importFrom stats sd time ts
#' @importFrom graphics points abline polygon text grid legend plot
#' @importFrom raster values
#' @importFrom forecast na.interp
#'
#' @param x Matrix, RasterStack or Rasterbrick without NA's.
#' @param startm The start of the monitoring time.
#' @param endm The end of the monitoring time.
#' @param threshold The default threshold is 5 for photosynthetic vegetation,
#' while for indices such as NDVI and EVI the threshold is 3.
#' Please see Tarazona et al. (2018) for more details.
#' @param img The image of the position immediately before the monitoring start,
#' i.e. the "start-1" position (in case "x" is a matrix). The matrix \code{x} and
#' the image \code{img} must have the same dimension (i.e. rows and cols).
#'
#' @param vf If the monitoring is with Photosynthetic Vegetation series,
#' then switch to \code{TRUE}.
#' @param verbose This paramater is Logical. It Prints progress messages during execution.
#'
#' @examples
#' library(ForesToolboxRS)
#' library(raster)
#' data(FTdata)
#'
#' # Detect changes in 2008 (position 19) using a raster(RasterStack)
#' cd <- pvtsRaster(x=serie_pv, startm=19, endm=19,  threshold= 5)
#'
#'@export
#'
pvtsRaster <- function(x, startm, endm, threshold = 5, img, vf = FALSE, verbose = FALSE) {

  if (is(x, "matrix")) {

    breakR <- img

    if(dim(breakR)[3]!=1) stop("img must have one band", call. = TRUE)

    if(dim(x)[1]!=dim(breakR)[1]) stop("x and img must have the same row", call. = TRUE)

    if(dim(x)[2]!=dim(breakR)[2]) stop("x and img must have the same col", call. = TRUE)

    if(verbose){
      message(paste0(paste0(rep("*",10), collapse = ""), " Calculating the mean, standard deviation and lower limit" , paste0(rep("*",10), collapse = "")))
      print(model_algo)
    }

    mean.pvts <- apply(x[,1:(startm-1)], 1, mean)
    std.ptvs <- apply(x[,1:(startm-1)], 1, sd)
    cd <- ifelse(x[,endm] < (mean.pvts - threshold*std.ptvs), 1, 0)
    values(breakR) <- cd

    # Photosynthetic vegetation?

    if (vf) {
      breakR[img < 50 | img < 0.5 | img < 5000] <- 0
    }

  } else if (is(x,'RasterStack') | is(x,'RasterBrick')) {

    img <- x[[endm-1]]
    breakR <- x[[1]]

    x <- raster::as.matrix(x)

    if(verbose){
      message(paste0(paste0(rep("*",10), collapse = ""), " Calculating the mean, standard deviation and lower limit" , paste0(rep("*",10), collapse = "")))
      print(model_algo)
    }

    mean.pvts <- apply(x[,1:(startm-1)], 1, mean)
    std.ptvs <- apply(x[,1:(startm-1)], 1, sd)
    cd <- ifelse(x[,endm] < (mean.pvts - threshold*std.ptvs), 1, 0)
    values(breakR) <- cd

    # Photosynthetic vegetation?

    if (vf) {
      breakR[img < 50 | img < 0.5 | img < 5000] <- 0
    }

  } else stop(class(x), ' class is not supported', call. = TRUE)

  return(breakR)

}
