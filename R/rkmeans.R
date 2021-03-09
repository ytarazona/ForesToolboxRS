#' Unsupervised classification - kmeans
#'
#' This function allows to classify satellite images using k-means.
#'
#' @references
#'
#' Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani. (2013).
#' An introduction to statistical learning : with applications in R. New York: Springer.
#'
#' @details If we want to find the optimal value of \code{k} (clusters or classes),
#' we should use \code{k = NULL}. This approach will look for the k values for
#' which the intra-class inertia is stabilized. If we know the \code{k} value and the idea
#' is to find the best algorithm embedded in k-means, that maximizes inter-class distances,
#' we must put \code{k = n}, where \code{n} is a specified number of classes.
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
#' # Selecting the best embedded algorithm in kmeans
#' classKmeans <- rkmeans(img = img_l8, k = 4, algo = "MacQueen")
#' @export
#'
rkmeans <- function(img, k, iter.max = 100, nstart = 50, algo = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), verbose = FALSE, ...){

  vr <- getValues(img)

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " Getting NA positions ", paste0(rep("*",10), collapse = "")))
  }

  i <- which(!is.na(vr)) # NA positions

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " Appling kmeans ", paste0(rep("*",10), collapse = "")))
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
