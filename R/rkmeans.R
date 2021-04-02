#' Unsupervised classification - kmeans
#'
#' This function allows to classify satellite images using k-means.
#'
#' @references
#'
#' Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani. (2013).
#' An introduction to statistical learning : with applications in R. New York: Springer.
#'
#' @details In principle, this function allows to classify satellite images specifying
#' a \code{k} value, however it is recommended to find the optimal value of \code{k} using
#' the \link[ForesToolboxRS]{calkmeans} function.
#'
#' @importFrom raster getValues raster
#'
#' @param img RasterStack or RasterBrick
#' @param k the number of clusters
#' @param iter.max The maximum number of iterations allowed
#' @param nstart if centers is a number, how many random sets should be chosen?
#' @param algo It can be "Hartigan-Wong", "Lloyd", "Forgy" or "MacQueen". See \link[stats]{kmeans}
#' @param verbose This paramater is Logical. It Prints progress messages during execution.
#' @param ... Options to be passed to the function. See \link[stats]{kmeans}
#'
#' @examples
#' library(ForesToolboxRS)
#'
#' # Load the dataset
#' data(img_l8)
#'
#' # Select the best embedded algorithm in kmeans
#' classKmeans <- rkmeans(img = img_l8, k = 4, algo = "MacQueen")
#' @export
#'
rkmeans <- function(img, k, iter.max = 100, nstart = 50, algo = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), verbose = FALSE, ...){

  vr <- getValues(img)

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " Get NA positions ", paste0(rep("*",10), collapse = "")))
  }

  i <- which(!is.na(vr)) # NA positions

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " Apply kmeans ", paste0(rep("*",10), collapse = "")))
  }

  # Applying kmeans
  km <- stats::kmeans(na.omit(vr), k, iter.max, nstart, algorithm = algo, ...)

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " Obtained raster ", paste0(rep("*",10), collapse = "")))
  }

  raster_class <- raster(img)
  raster_class[i] <- km$cluster

  names(raster_class) <- "kmeans_class"
  return(raster_class)
}
