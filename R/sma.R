#' Spectral Mixture Analysis (SMA)
#'
#' The SMA assumes that the energy received within the field of vision of the
#' remote sensor can be considered as the sum of the energies received from each
#' dominant endmember. This function addresses a Linear Mixing Model.
#'
#' @references
#' Adams, J. B., Smith, M. O., & Gillespie, A. R. (1993). Imaging spectroscopy:
#' Interpretation based on spectral mixture analysis. In C. M. Pieters & P.
#' Englert (Eds.), Remote geochemical analysis: Elements and mineralogical
#' composition. NY: Cambridge Univ. Press 145-166 pp.
#'
#' Shimabukuro, Y.E. and Smith, J., (1991). The least squares mixing models to
#' generate fraction images derived from remote sensing multispectral data.
#' IEEE Transactions on Geoscience and Remote Sensing, 29, pp. 16-21.
#'
#' @details
#' A regression analysis is used to obtain the fractions. In least squares
#' inversion algorithms, the common objective is to estimate abundances that
#' minimize the squared error between the actual spectrum and the estimated spectrum.
#' The values of the fractions will be between 0 and 1.
#'
#' @param img Optical images. It could be RasterStack or RasterBrick.
#' @param endm Endmembers must be a matrix or data.frame and with more than one endmember.
#' Rows represent the endmembers and columns represent the spectral bands.  The number
#' of bands must be greater than the number of endmembers.
#' @param verbose This parameter is Logical. It Prints progress messages during execution.
#'
#' @importFrom raster as.matrix
#'
#' @examples
#' library(ForesToolboxRS)
#' library(raster)
#'
#' # Load an example dataset
#' data(img_l8)
#'
#' # Endmembers (Surface reflectance between 0 and 10000)
#' soil <- c(8980, 8508, 8704, 13636, 16579, 11420)
#' forest <- c(8207, 7545, 6548, 16463, 9725, 6673)
#' water <- c(9276, 9570, 10089, 6743, 5220, 5143)
#' endmembers <- matrix(c(soil, forest, water), 3, 6,
#'   byrow = TRUE, dimnames =
#'     list(c("soil", "forest", "water"), c("B1", "B2", "B3", "B4", "B5", "B6"))
#' )
#'
#' # Unmix the image
#' fractions <- sma(img = img_l8, endm = endmembers)
#' plotRGB(fractions, 1, 2, 3, stretch = "lin")
#' @export
#'
sma <- function(img, endm, verbose = FALSE) {
  if (inherits(img, "RasterStack") | inherits(img, "RasterBrick")) {
    df <- as.matrix(img)
  } else {
    stop(class(img), " This class is not supported yet.", call. = TRUE)
  }

  if (verbose) {
    message(paste0(paste0(rep("*", 10), collapse = ""), " Obtaining fractions through least squares ", paste0(rep("*", 10), collapse = "")))
  }

  if (dim(df)[2] > dim(endm)[1]) {
    if (dim(df)[2] == dim(endm)[2]) {

      # Transposed from the endmembers matrix
      M <- t(endm)

      # We calculate fractions through least squares
      lmm <- matrix(nrow = ncol(M), ncol = dim(df)[1])
      for (i in seq_len(ncol(lmm))){
        lmm[, i] = tcrossprod((solve(crossprod(M))), M) %*% df[i, ]
      }
      val <- t(lmm)

      if (verbose) {
        message(paste0(paste0(rep("*", 10), collapse = ""), " Obtaining Root Mean Square Error ", paste0(rep("*", 10), collapse = "")))
      }

      # We estimate the RMSE
      fra_esti <- lapply(1:dim(val)[1], function(i) f <- M %*% val[i, ])
      m_df <- lapply(as.list(1:dim(df)[1]), function(x) df[x[1], ])
      rmse <- mapply(function(x, y) sqrt(mean((x - y)^2)), m_df, fra_esti)

      if (verbose) {
        message(paste0(paste0(rep("*", 10), collapse = ""), " Save the fractions on a raster ", paste0(rep("*", 10), collapse = "")))
      }

      # We store the fractions on a raster
      fractions <- cbind(val, rmse)

      sma_fractions <- img[[1:(dim(endm)[1] + 1)]]
      for (j in 1:(dim(endm)[1] + 1)) {
        sma_fractions[[j]][] <- fractions[, j]
      }

      names(sma_fractions) <- c(as.vector(row.names(endm)), "RMSE")

    } else {
      stop(" The number of values extracted in band should be equal.", call. = TRUE)
    }
  } else {
    stop(" The number of bands must be greater than the number of endmembers.", call. = TRUE)
  }

  return(sma_fractions)
}
