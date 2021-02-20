#' Fusion of images with different observation geometry
#'
#' This algorithm allows to fusion images coming from different spectral sensors
#' (e.g., optical-optical, optical and SAR or SAR-SAR). Among many of the qualities
#' of this function, it is possible to obtain the contribution (%) of each variable
#' in the fused image.
#'
#' @author Yonatan Tarazona
#'
#' @section Value:
#' A list with Fused images (RasterStack), Variance, Proportion of variance, Cumulative variance,
#' Correlation and Contribution in percentage.
#'
#' @section Note:
#' Before executing the function, it is recommended that images coming from different
#' sensors or from the same sensor have a co-registration.
#'
#' @importFrom stats prcomp na.omit
#' @importFrom raster getValues as.data.frame brick extent plotRGB
#' @importFrom factoextra get_pca_var
#'
#' @param x Optical image. It could be RasterStack or RasterBrick.
#' @param y Radar image. It could be RasterStack or RasterBrick.
#' @param stand.varb Logical. If \code{TRUE}, the PCA is calculated using the correlation
#' matrix (standardized variables) instead of the convariance matrix (non-standardized variables).
#' @param na If \code{TRUE} the NA values of the images will be omitted from the analysis.
#' @param verbose This paramater is Logical. It Prints progress messages during execution.
#'
#' @examples
#' \dontrun{
#' library(ForesToolboxRS)
#' library(raster)
#'
#' # Load an example dataset
#' data(FTdata)
#'
#' # Fusing optical and radar data
#' fusion <- fusionRS(x=optical, y=radar, na = TRUE)
#' plotRGB(fusion[[1]], 1,2,3, axes=F, stretch="lin",main ="Fused images")
#' }
#' @export
#'
fusionRS <- function(x, y, stand.varb = TRUE, na = FALSE, verbose = FALSE) {

  if (is(x,'stars')) {
    x <- brick(mapply(function(z) as(x[z],'Raster'), seq_len(length(x))))
  }

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " Verifying the same extent " , paste0(rep("*",10), collapse = "")))
    print(model_algo)
  }

  if (is(x, "RasterStack") | is(x, "RasterBrick") & is(y, "RasterStack") | is(y, "RasterBrick")) {

    if (extent(x) == extent(y)){
      img <- stack(x, y)
    } else {
      stop(" The extent of the images are different.", call. = TRUE)
    }
  } else {

    stop(c(class(x),class(y)), " This classes are not supported yet.", call. = TRUE)
  }

  df <- as.data.frame(img)

  # Omit NA
  if (na) {
    df <- na.omit(df)
  }

  # standardized variables?
  if (stand.varb) {

    if(verbose){
      message(paste0(paste0(rep("*",10), collapse = ""), " Standardizing variables " , paste0(rep("*",10), collapse = "")))
      print(model_algo)
    }

    acp <- prcomp(df, center = TRUE, scale = TRUE) # standardized variables
  } else {
    acp <- prcomp(df, center = FALSE, scale = FALSE) # non-standardized variables
  }

  if(verbose){
    message(paste0(paste0(rep("*",10), collapse = ""), " Contributions and correlations " , paste0(rep("*",10), collapse = "")))
    print(model_algo)
  }

  var <- summary(acp)$importance[1, ]^2 # Variance
  pov <- summary(acp)$importance[2, ] # Proportion of variance
  varAcum <- summary(acp)$importance[3, ] # Cumulative variance
  corr <- get_pca_var(acp)$cor # Correlation
  contri <- get_pca_var(acp)$contrib # Contribution in %

  for (i in 1:dim(df)[2]) {
    colnames(corr)[i] <- paste("PC", i, sep = "")
    rownames(corr)[i] <- paste("Band", i, sep = "")
    colnames(contri)[i] <- paste("PC", i, sep = "")
    rownames(contri)[i] <- paste("Band", i, sep = "")
  }

  # Storing in a raster the principal components
  acpY <- img
  vr <- getValues(acpY)
  p <- which(!is.na(vr)) # Positions of NA

  # We assign a raster format to each PC
  for (k in 1:dim(df)[2]) {
    acpY[[k]][p] <- acp$x[, k]
  }

  results <- list(FusionRS = acpY, var = var, pov = pov, varAcum = varAcum, corr = corr, contri = contri)

  names(results) <- c("Fused_images", "Variance", "Proportion_of_variance",
                      "Cumulative_variance", "Correlation", "Contribution_in_pct")

  return(structure(results, class="fusionRS"))
}

#' Print for the "fusionRS" class
#'
#' @param x Object of class "fusionRS".
#'
#' @export
#'
print.fusionRS <- function(x){
  cat("******************** ForesToolboxRS - FUSION OF IMAGES ********************\n")
  cat("\n**** Fused images ****\n")
  show(x$Fused_images)
  cat("\n**** Variance ****\n")
  print(x$Variance)
  cat("\n**** Proportion_of_variance ****\n")
  print(x$Proportion_of_variance)
  cat("\n**** Cumulative_variance ****\n")
  print(x$Cumulative_variance)
  cat("\n**** Correlation ****\n")
  print(x$Correlation)
  cat("\n**** Contribution_in_percentage ****\n")
  print(x$Contribution_in_pct)
}
