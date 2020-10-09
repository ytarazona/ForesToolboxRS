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
#' In order to optimise the detections, it is advisable to make a smoothing
#' through the \link[ForesToolboxRS]{smootH} function before detecting changes. The smoothing will
#' allow to eliminate outliers that were not eliminated during the masking. If the object is a
#' time series, it must have the structure of a "ts", that is, the serie must have a start, end
#' and frequency. See \link[stats]{ts} for more details. In addition, in case the input is a
#' matrix, the first dimension must be rows*columns of the image, and the second dimension the
#' number of images.
#'
#' @importFrom stats sd time ts
#' @importFrom graphics points abline polygon text grid legend plot
#' @importFrom raster values
#' @importFrom forecast na.interp
#'
#' @param x Matrix without NA's, RasterStack or Rasterbrick.
#' @param startm The start of the monitoring time.
#' @param endm The end of the monitoring time.
#' @param threshold The default threshold is 5 for photosynthetic vegetation,
#' while for indices such as NDVI and EVI the threshold is 3.
#' Please see Tarazona et al. (2018) for more details.
#' @param img The image of the position immediately before the monitoring start,
#' i.e. the "start-1" position (in case "x" is a matrix).
#' @param vf If the monitoring is with Photosynthetic Vegetation series,
#' then switch to \code{TRUE} (in case "x" is a matrix).
#' @param verbose This paramater is Logical. It Prints progress messages during execution.
#'
#' @export
#'
#' @examples
#' library(ForesToolboxRS)
#'
#' # Example 1.
#' # photosynthetic vegetation from 1990 to 2017, one for each year
#' vec <- c(0.86,0.93,0.97,0.91,0.95,0.96,0.91,0.88,0.92,0.89,
#'          0.90,0.89,0.91,0.92,0.89,0.90,0.92,0.84,0.46,0.20,
#'          0.27,0.22,0.52,0.63,0.61,0.67,0.64,0.86)
#'
#' # Detect changes in 2008 (position 19)
#' cd <- pvts(x=vec, startm=19, endm=19, threshold= 5)
#'
#' # Detect changes in 2008 (time serie)
#' vec <- ts(vec, start= 1990, end = 2017, frequency = 1)
#' cd <- pvts(x=vec, startm=2008, endm=2008,  threshold= 5)
#'
pvtsRaster <- function(x, startm, endm, threshold = 5, img, vf = FALSE, verbose = FALSE) {

  if (is(x, "matrix")) {

    breakR <- img

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

    x <- as.matrix(x)

    if(verbose){
      message(paste0(paste0(rep("*",10), collapse = ""), " Calculating the mean, standard deviation and lower limit" , paste0(rep("*",10), collapse = "")))
      print(model_algo)
    }

    breakR <- img
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
