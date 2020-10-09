#' Gain and loss in land cover classification
#'
#' This algorithm is able to obtain gain and loss in land cover classification.
#'
#' @author Yonatan Tarazona
#'
#' @section Value:
#' A list with loss raster, gain raster, loss hectares and gain hectares.
#'
#' @section Note:
#' It is important to mention that the images used to obtain gain and loss in land cover classification
#' must have the same extent. It is preferable that the classified images come from the same software.
#'
#' @param x First classified image.
#' @param y Second classified image of a time later.
#'
#' @examples
#' library(ForesToolboxRS)
#' library(raster)
#'
#' rastClas1 <- raster(ncol=10, nrow=10, xmn=-1000, xmx=1000, ymn=-100, ymx=900)
#' values(rastClas1) <- floor(runif(100, min = 1, max = 5))
#'
#' rastClas2 <- raster(ncol=10, nrow=10, xmn=-1000, xmx=1000, ymn=-100, ymx=900)
#' values(rastClas2) <- floor(runif(100, min = 1, max = 5))
#'
#' change <- coverChange(rastClas1, rastClas2)
#'
#' @export
#'
coverChange <- function(x, y){

  if(class(try(stack(x, y), silent = TRUE)) == "try-error") stop("x and y must have the same extent.", call. = TRUE)

  mask <- x - y
  mask[mask!= 0] <- 1
  mask[mask == 0] <- NA

  loss <- mask(x, mask)
  gain <- mask(y, mask)

  loss_ha <- lapply(1:max(raster::values(x), na.rm = TRUE),
                    function(i) {sum_ha <- sum(x[x == i])*raster::res(x)[1]^2/10000}
                    )

  gain_ha <- lapply(1:max(raster::values(y), na.rm = TRUE),
                    function(j) {sum_ha <- sum(y[y == j])*raster::res(y)[1]^2/10000}
                    )

  result <- list(loss, gain, as.numeric(loss_ha), as.numeric(gain_ha))

  names(result) <- c("LossRaster_x", "GainRaster_y", "Loss_hectare_x", "Gain_hectare_y")

  return(result)

}
