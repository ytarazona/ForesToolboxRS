#' Smoothing time series
#'
#' Temporary smoothing is used in order to eliminate outliers in the time series.
#'
#' @references
#' Tarazona, Y., Mantas, V.M., Pereira, A.J.S.C. (2018). Improving tropical
#' deforestation detection through using photosynthetic vegetation time
#' series (PVts-\eqn{\beta}). Ecological Indicators, 94, 367 379.
#'
#' Hamunyela, E., Verbesselt, J., Roerink, G., & Herold, M. (2013).
#' Trends in spring phenology of western European deciduous forests.
#' Remote Sensing,5(12), 6159-6179.
#'
#' @details Vegetation indices or fractions of photosynthetic activity generally present
#' noise in the time series that in some way or another hinder subsequent analyzes.
#' This noise can be due to two reasons: i) no algorithm was used to mask the atmospheric
#' noise before analyzing the time series and ii) negative outliers that were not detected
#' by the masking algorithm. To minimize this problem, it is possible to eliminate negative
#' outliers using the method proposed by [Hamunyela et al. (2013)](https://doi.org/10.3390/rs5126159).
#'
#' This method is not able to remove consecutive outliers. The mathematical approach
#' of this method of removing outliers implies the non-modification of the first and last
#' values of the historical series, so that the near real-time detections of ecosystem
#' disturbances will not be affected in any way.
#'
#' @importFrom forecast na.interp
#' @importFrom zoo na.approx na.spline
#'
#' @param x Numeric, matrix.
#' @param na ??
#' @param method.interp Four interpolation methods are presented, "na.interp",
#' "na.approx" and "na.spline". By default is the method "na.interp".
#'
#' @examples
#'
#' library(ForesToolboxRS)
#'
#' # Here a vector with an outlier
#' x <- c(80, 78, 75, 76, 79, -100, 82, 76, 81, 77, 76)
#' plot(x, type = "o", col = "red")
#'
#' # Applying a smoothing
#' smth <- smootH(x)
#' plot(x, type = "o", ylab = "Reflectance %", xlab = "Time")
#' lines(smth, col = "blue", type = "o")
#' @export
#'
smootH <- function(x, na = FALSE, method.interp = "na.interp"){

  if (inherits(x, 'numeric')) {

    if (na){
      #x[x <= -1 | x== -1] <- NA
      x[sum(is.na(x)) >= (length(x)-1)] <- sample(99:100, 1) # These data are evidently masked areas or bodies of water, so that it is just to fill out data
      # Type of interpolation
      if (method.interp == "na.interp") {
        x <- na.interp(x)
      } else if (method.interp == "na.approx") {
        x <- na.approx(x)
      } else if (method.interp == "na.spline") {
        x <- na.spline(x)
      } else {
        stop("Unsupported interpolation method.", call. = TRUE)
      }
    }

    # Apply Hamunyela Smoothing
    for (j in 2:(length(x) - 1)) {
      x[j] <- ifelse(((x[j] - x[j - 1]) < -0.01 * x[j - 1]) & ((x[j] - x[j + 1]) < -0.01 * x[j + 1]),
        (x[j - 1] + x[j + 1]) / 2, x[j]
      )
    }

    np <- as.numeric(x)

  } else if (inherits(x, 'matrix')) {

    if (na) {
      for (i in 1:dim(x)[1]) {
        #x[i,][x[i,] <= -1 | x[i,] == -1] <- NA
        x[i,][sum(is.na(x[i,])) >= (dim(x)[2]-1)] <- sample(99:100, 1) # These data are evidently masked areas or bodies of water, so that it is just to fill out data

        # Type of interpolation
        if (method.interp == "na.interp") {
          x[i, ] <- na.interp(x[i, ])
        } else if (method.interp == "na.approx") {
          x[i, ] <- na.approx(x[i, ])
        } else if (method.interp == "na.spline") {
          x[i, ] <- na.spline(x[i, ])
        } else {
          stop("Unsupported interpolation method.", call. = TRUE)
        }
      }
    }

    # Apply Hamunyela Smoothing
    for (i in 1:dim(x)[1]) {
      for (j in 2:(dim(x)[2] - 1)) {
        x[i, ][j] <- ifelse(((x[i, ][j] - x[i, ][j - 1]) < -0.01 * x[i, ][j - 1]) & ((x[i, ][j] - x[i, ][j + 1]) < -0.01 * x[i, ][j + 1]),
          (x[i, ][j - 1] + x[i, ][j + 1]) / 2, x[i, ][j]
        )
      }
    }

    np <- x
  } else {

    stop(class(x), ' class is not supported. It must be numeric (vector) or matrix.', call. = TRUE)
  }

  return(np)

}
