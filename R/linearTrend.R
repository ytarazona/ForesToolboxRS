#' Linear Trend in Remote Sensing
#'
#' Linear trend is useful for mapping forest degradation, land degradation, among others.
#' This algorithm is capable of obtaining the slope of an ordinary least-squares
#' linear regression and its reliability (p-value).
#'
#' @author Yonatan Tarazona
#'
#' @section Details:
#' Linear regression is widely used to analyze forest degradation or land degradation.
#' Specifically, the slope and its reliability are parameters for this analysis and these
#' can be obtained whit this function. On the other hand, logistic regression allows
#' obtaining a degradation risk map, in other words, it is a probability map. Please see
#' the references or see [Tarazona and Maria-Miyasiro (2020)](https://doi.org/10.1016/j.rsase.2020.100337).
#'
#' @section References:
#' Tarazona, Y., Maria, Miyasiro-Lopez. (2020). Monitoring tropical forest degradation using
#' remote sensing. Challenges and opportunities in the Madre de Dios region, Peru. Remote
#' Sensing Applications: Society and Environment, 19, 100337.
#'
#' Wilkinson, G.N., Rogers, C.E., 1973. Symbolic descriptions of factorial models for
#' analysis of variance. Appl. Stat. 22, 392-399.
#'
#' Chambers, J.M., 1992. Statistical Models in S. CRS Press.
#'
#' @importFrom dplyr bind_rows
#'
#' @param x RasterStack, RasterBrick or Matrix (row*col, n_images).
#' @param type There are two options: "lm" for linear regressions and "glm" for Fitting
#' Generalized Linear Models.
#' @param ... For "lm" and "glm": arguments to be used to form the default control argument
#' if it is not supplied directly. See \link[stats]{lm} and \link[stats]{glm}.
#'
#' @export
#'
#' @examples
#' library(ForesToolboxRS)
#' library(raster)
#'
#'e <- extent(350420.9,352028.8,-1417869,-1416288)
#'imgs <- crop(img_pv, e)
#'trend <- linearTrend(x = imgs)
#'plot(trend[[1]]) # raster of slope
#'plot(trend[[2]]) # raster of p-value
#'

linearTrend <- function(x, type = "lm", ...){

  if (type == "lm") {
    if (is(x,'RasterStack') | is(x,'RasterBrick')) {

      mat <- raster::as.matrix(x)

    } else if (is(x, "matrix")) {
      mat <- x
    } else {

      stop(class(x), ' class is not supported', call. = TRUE)
    }

    x_axis <- 1:dim(mat)[2]
    my_lms <- lapply(1:dim(mat)[1], function(i) reg <- lm(mat[i,] ~ x_axis, ...))
    summy <- suppressWarnings(
      lapply(my_lms, function(x) summary(x)$coefficients[2, 1:4]))

    summy <- as.data.frame(bind_rows(summy))

    if (is(x,'RasterStack') | is(x,'RasterBrick')) {

      slope <- raster(x[[1]])
      p_value <- raster(x[[2]])

      slope[] <- summy[,1]
      p_value[] <- summy[,4]
      result <- stack(slope, p_value)
      names(result) <- c("slope", "p_value")

      return(result)

    } else if (is(x,'matrix')) {

      result <- array(c(summy[,1], summy[,4]), c(dim(x)[1], dim(x)[2], 2))

      return(result)

    }

  } else if (type == "glm") {

    if (is(x,'RasterStack') | is(x,'RasterBrick')) {

      names(x[[1]]) <- c("FD")
      df <- raster::as.matrix(x)

      mod <- glm(FD~., data = df, ...)

      prob <- predict(mod, type = "response")

      result <- raster(x[[1]])
      result[] <- prob
      names(result) <- c("MapProbability")

      return(result)

    } else if (is(x, "list")) {

      if(class(try(stack(x), silent = TRUE)) == "try-error"){

        master <- raster(x[1])
        meta <- raster(extent(master), nrows = master@nrows, ncols = master@ncols)

        varib_rast <- c()
        for(k in 1:length(x)){
          ras_ini <- raster(x[[k]])
          ras_post <- resample(ras_ini, meta, method = "ngb")
          ras_post[is.na(ras_post)] <- 0
          varib_rast <- c(varib_rast, ras_post)
        }

        x <- varib_rast
        names(x[[1]]) <- c("FD")

      }else {

        x <- stack(x)
        names(x[[1]]) <- c("FD")

      }

      df <- raster::as.matrix(x)

      mod <- glm(FD~., data = df, ...)
      prob <- predict(mod, type="response")

      result <- master
      result[] <- prob

      names(result) <- c("MapProbability")

      return(result)

    } else {

      stop(class(x), ' class is not supported', call. = TRUE)
    }


  } else {

    stop(' Method is not supported', call. = TRUE)
  }

}
