#' Unsupervised classification - kmeans
#'
#' This function allows to classify satellite images using k-means.
#'
#' @author Yonatan Tarazona
#'
#' @section References:
#'
#' Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani. (2013).
#' An introduction to statistical learning : with applications in R. New York: Springer.
#'
#' @section Details: If we want to find the optimal value of \code{k} (clusters or classes),
#' so we must put \code{k = NULL} as an argument of the function. Here, we are finding k for
#' which the intra-class inertia is stabilized. If we know the \code{k} value and the idea
#' is to find the best algorithm embedded in kmeans, that maximizes inter-class distances,
#' we must put \code{k = n}, where \code{n} is a specific class number.
#'
#' @importFrom raster getValues raster
#'
#' @param img RasterStack or RasterBrick.
#' @param k the number of clusters.
#' @param iter.max The maximum number of iterations allowed.
#' @param nstart if centers is a number, how many random sets should be chosen?.
#' @param algo It can be "Hartigan-Wong", "Lloyd", "Forgy" or "MacQueen". See \link[stats]{kmeans}.
#' @param verbose This paramater is Logical. It Prints progress messages during execution.
#' @param ... Options to be passed to the function. See \link[stats]{kmeans}.
#'
#' @examples
#' \dontrun{
#' library(ForesToolboxRS)
#'
#' # Load the dataset
#' data(FTdata)
#'
#' # Selecting the best embedded algorithm in kmeans
#' classKmeans <- rkmeans(img = image, k = 4, algo = "MacQueen")
#' plot(classKmeans)
#' }
#' @export
#'
rkmeans <- function(img, k, iter.max = 100, nstart = 50, algo = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), ...){

  vr <- getValues(img)
  i <- which(!is.na(vr)) # NA positions

  # Applying kmeans
  km <- kmeans(na.omit(vr), k, iter.max, nstart, algorithm = algo, ...)

  raster_class <- raster(img)
  raster_class[i] <- km$cluster

  names(raster_class) <- "kmeans_class"
  return(raster_class)

}
