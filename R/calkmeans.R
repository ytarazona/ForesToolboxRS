#' Calibrating kmeans
#'
#' This function allows to calibrate the kmeans algorithm. It is possible to obtain
#' the best k value and the best embedded algorithm in kmeans.
#'
#' @author Yonatan Tarazona
#'
#' @section References:
#' Tarazona, Y., Mar?a, Miyasiro-L?pez. (2020). Monitoring tropical forest degradation using
#' remote sensing. Challenges and opportunities in the Madre de Dios region, Peru. Remote
#' Sensing Applications: Society and Environment, 19, 100337.
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
#' @importFrom raster getValues
#'
#' @param img RasterStack or RasterBrick.
#' @param k This argument is \code{NULL} when the objective is to obtain the bes \code{k} value. If the objective
#' is to select the best algorithm embedded in kmeans, please especify a \code{k} value.
#' @param iter.max The maximum number of iterations allowed. See \link[stats]{kmeans}.
#' @param algo It can be "Hartigan-Wong", "Lloyd", "Forgy" or "MacQueen". See \link[stats]{kmeans}.
#' @param iter Iterations number to obtain the best k value. \code{iter} must be greater than the number of classes
#' expected to be obtained in the classification. Default is 30.
#' @param verbose This paramater is Logical. It Prints progress messages during execution.
#' @param ... Options to be passed to the function. See 'Details'.
#'
#' @examples
#' library(ForesToolboxRS)
#'
#' # Load the dataset
#' data(FTdata)
#'
#' # Selecting the best k value
#' best_k <- calkmeans(img = img[[1:2]], k = NULL, iter.max = 10,
#'            algo = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), iter = 30)
#' # Jambu Elbow
#' plot(best_k)
#'
#'
#'# Selecting the best embedded algorithm in kmeans
#' best_algo <- calkmeans(img = img[[1:2]], k = 4, iter.max = 10,
#'            algo = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"), iter = 30)
#'
#' @export
#'

calkmeans <- function(img, k = NULL, iter.max = 10, algo = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"),
                      iter = 30, verbose = FALSE, ...){

  if(!inherits(img, "Raster")) stop("img must be a RasterBrick or RasterStack", call. = TRUE)

  if(is.null(k)) {

    algo_test <- c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueenn")

    if (!algo %in% algo_test) stop("Unsupported algorithm. \nAlgortihm must be Hartigan-Wong, Lloyd, Forgy or MacQueen", call. = TRUE)

    vr <- getValues(img)

    if ("Hartigan-Wong" %in% algo){

      IntraIC.Hartigan_wong <- rep(0, iter)
      for (k in 1:iter) {
        grupos <- kmeans(na.omit(vr), k, iter.max = iter.max, algorithm = "Hartigan-Wong", ...)
        IntraIC.Hartigan_wong[k] <- grupos$tot.withinss
      }

      vecIner.hw <- list(IntraIC.Hartigan_wong = IntraIC.Hartigan_wong)
    } else {
      vecIner.hw <- list()
    }

    if ("Lloyd" %in% algo){

      IntraIC.Lloyd <- rep(0, iter)
      for (k in 1:iter) {
        grupos <- kmeans(na.omit(vr), k, iter.max = iter.max, algorithm = "Lloyd", ...)
        IntraIC.Lloyd[k]<-grupos$tot.withinss
      }

      vecIner.l <- list(IntraIC.Lloyd = IntraIC.Lloyd)
    } else {
      vecIner.l <- list()
    }

    if ("Forgy" %in% algo){

      IntraIC.Forgy <- rep(0, iter)
      for (k in 1:iter) {
        grupos <- kmeans(na.omit(vr), k, iter.max = iter.max, algorithm = "Forgy", ...)
        IntraIC.Forgy[k] <- grupos$tot.withinss
      }

      vecIner.f <- list(IntraIC.Forgy = IntraIC.Forgy)
    } else {
      vecIner.f <- list()
    }

    if ("MacQueen" %in% algo){

      IntraIC.MacQueen <- rep(0, iter)
      for (k in 1:iter) {
        grupos <- kmeans(na.omit(vr), k, iter.max = iter.max, algorithm = "MacQueen", ...)
        IntraIC.MacQueen[k] <- grupos$tot.withinss
      }

      vecIner.m <- list(IntraIC.MacQueen = IntraIC.MacQueen)
    } else {
      vecIner.m <- list()
    }

    resulFinal <- c(vecIner.hw, vecIner.l, vecIner.f, vecIner.m)

    return(structure(resulFinal, class = "calkmeans"))


  } else if (inherits(k, "numeric")) {

    algo_test <- c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueenn")

    if (!algo %in% algo_test) stop("Unsupported algorithm. \nAlgortihm must be Hartigan-Wong, Lloyd, Forgy or MacQueen", call. = TRUE)

    vr <- getValues(img)

    if ("Hartigan-Wong" %in% algo){

      InterIC.Hartigan_wong <- 0
      for (k in 1:iter) {
        grupos <- kmeans(na.omit(vr), k, iter.max = iter.max, algorithm = "Hartigan-Wong", ...)
        InterIC.Hartigan_wong <- InterIC.Hartigan_wong+grupos$betweenss
      }

      InterIC.Hartigan_wong <- InterIC.Hartigan_wong/iter

      vecIner.hw <- list(InterIC.Hartigan_wong = InterIC.Hartigan_wong)
    } else {
      vecIner.hw <- list()
    }

    if ("Lloyd" %in% algo){

      InterIC.Lloyd <- 0
      for (k in 1:iter) {
        grupos <- kmeans(na.omit(vr), k, iter.max = iter.max, algorithm = "Lloyd", ...)
        InterIC.Lloyd <- InterIC.Lloyd+grupos$betweenss
      }

      InterIC.Lloyd <- InterIC.Lloyd/iter

      vecIner.l <- list(InterIC.Lloyd = InterIC.Lloyd)
    } else {
      vecIner.l <- list()
    }

    if ("Forgy" %in% algo){

      InterIC.Forgy <- 0
      for (k in 1:iter) {
        grupos <- kmeans(na.omit(vr), k, iter.max = iter.max, algorithm = "Forgy", ...)
        InterIC.Forgy <- InterIC.Forgy+grupos$betweenss
      }

      InterIC.Forgy <- InterIC.Forgy/iter

      vecIner.f <- list(InterIC.Forgy = InterIC.Forgy)
    } else {
      vecIner.f <- list()
    }

    if ("MacQueen" %in% algo){

      InterIC.MacQueen <- 0
      for (k in 1:iter) {
        grupos <- kmeans(na.omit(vr), k, iter.max = iter.max, algorithm = "MacQueen", ...)
        InterIC.MacQueen <- InterIC.MacQueen+grupos$betweenss
      }

      InterIC.MacQueen <- InterIC.MacQueen/iter

      vecIner.m <- list(InterIC.MacQueen = InterIC.MacQueen)
    } else {
      vecIner.m <- list()
    }

    resulFinal <- c(vecIner.hw, vecIner.l, vecIner.f, vecIner.m)

    return(resulFinal)

  }

}




#' Plot for the "calkmeans" class
#'
#'
plot.calkmeans <- function(x, xlab, ylab, type, main, cex, ...){

  xlabel <- missing(xlab)
  ylabel <- missing(ylab)
  title <- missing(main)
  tl <- missing(type)
  size <- missing(cex)

  if (xlabel)
    xlab <- "Clusters (Number of iterations)"

  if (ylabel)
    ylab <- "Intra-class Inertia"

  if (title)
    main <- "Finding k for which the intra-class inertia is stabilized"

  if (tl)
    type <- "b"

  if (size)
    cex <- 1.5

  maxVal <- max(sapply(x, max))

  plot(0,
       xlim = c(0.5, length(best_k[[1]]) + 0.5),
       ylim = c(0, maxVal),
       xlab = xlab,
       ylab = ylab,
       main = main,
       cex = 0)
  grid()

  names_algo <- names(x)

  if ("IntraIC.Hartigan_wong" %in% names_algo){

    lines(x$IntraIC.Hartigan_wong, col = "green", type = type, cex = cex)
    name_hw <- "Hartigan-Wong"
    color_hw <- "blue"

  } else {
    name_hw <- c()
    color_hw <- c()
  }

  if ("IntraIC.Lloyd" %in% names_algo){

    lines(x$IntraIC.Lloyd, col = "red", type = type, cex = cex)
    name_l <- "Lloyd"
    color_l <- "red"

  } else {
    name_l <- c()
    color_l <- c()
  }

  if ("IntraIC.Forgy" %in% names_algo){

    lines(x$IntraIC.Forgy, col = "magenta", type = type, cex = cex)
    name_f <- "Forgy"
    color_f <- "green"

  } else {
    name_f <- c()
    color_f <- c()
  }

  if ("IntraIC.MacQueen" %in% names_algo){

    lines(x$IntraIC.MacQueen, col = "blue", type = type, cex = cex)
    name_m <- "MacQueen"
    color_m <- "magenta"

  } else {
    name_m <- c()
    color_m <- c()
  }

  legend <- c(name_hw, name_l, name_f, name_m)
  color <- c(color_hw, color_l, color_f, color_m)

  legend("topright", inset = .02, legend = legend, col = color, lty = 1,lwd = 1, cex = 0.8)

}
