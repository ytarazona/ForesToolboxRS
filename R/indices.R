#' Remote sensing spectral indices
#'
#' This function allows to obtain several remote sensing spectral indices in the optical
#' domain.
#'
#' @references
#' Rouse, J.W., Hass, R.H., Schell, J.A., and Deering, D.W. (1973). Monitoring vegetation systems in the
#' Great Plains with ERTS. Proceedings of the Third ERTS Symposium, 1.
#'
#' Huete, A.R. (1988). A Soil-Adjusted Vegetation Index (SAVI). Remote Sensing of
#' Environment, 25,295-309.
#'
#' Huete, A.R., Justice, C., Liu, H. (1994). Development of vegetation and soil indices for MODIS-EOS.
#' Remote Sensing of Environment, 49, 224-234.
#'
#' Gao, B.C. (1996). NDWI—A normalized difference water index for remote sensing of vegetation
#' liquid water from space. Remote Sensing of Environment, 58, 257-266.
#'
#' Key, C.H.; Benson, N.C. Landscape Assessment: Ground Measure of Severity, the Composite
#' Burn Index, and Remote Sensing of Severity, the Normalized Burn Index. In FIREMON:
#' Fire Effects Monitoring and Inventory System; Lutes, D., Keane, R., Caratti, J., Key,
#' C.H., Benson, N.C., Sutherland, S., Gangi, L., Eds.; Rocky Mountains Research Station,
#' USDA Forest Service: Fort Collins, CO, USA, 2005.
#'
#' Camps-Valls, G., et al. (2021). A unified vegetation index for quantifying the terrestrial
#' biosphere. Science Advances, 7(9).
#'
#' @note
#' Currently implemented for satellites such as Landsat-4 TM, Landsat-5 TM, Landsat-7 ETM+,
#' Landsat-8 OLI and Sentinel-2, and any other sensor who has the appropriate bands. It is
#' possible to obtain spectral indices through digital number or top of atmosphere reflectance
#' (toa), however in order to calculate indices such as SAVI or EVI, the the data must be in
#' surface reflectance.
#'
#' @param index Character, index name.
#' @param blue Blue band, raster object.
#' @param green Green band, raster object.
#' @param red Red band, raster object.
#' @param nir Near-infrared band, raster object.
#' @param redEdge Red-edge band, raster object.
#' @param swir Short-wave-infrared band, raster object.
#' @param coefs List of coefficients to be used in some indices (Lsavi = 0.5, G = 2.5,
#' C1 = 6, C2 = 7.5, Levi = 1).
#'
#' @examples
#' library(ForesToolboxRS)
#' library(raster)
#'
#' # Load an example dataset
#' data(img_l8)
#'
#' # Obtain bands
#' nirBand <- img_l8[[4]]
#' redBand <- img_l8[[3]]
#'
#' # Obtain NDVI
#' ndvi <- indices(index = "NDVI", nir = nirBand, red = redBand)
#' plot(ndvi)
#'
#' @export
#'
indices <- function(index, blue = NULL, green = NULL, red = NULL, nir = NULL, redEdge = NULL,
                    swir = NULL, coefs = list(Lsavi = 0.5,  G = 2.5, C1 = 6, C2 = 7.5, Levi = 1)) {

  layersR <- c(class(blue), class(green), class(red), class(nir), class(redEdge), class(swir))

  if (any(layersR == "RasterBrick" | layersR == "RasterStack")) stop("image must be a Raster object (RasterLayer). One band", call. = TRUE)

  if (index == "NDVI") {

    indx <- (nir - red)/(nir + red)
    names(indx) <- "NDVI"

  } else if (index == "SAVI") {

    indx <- (1 + coefs[['Lsavi']])*(nir - red)/(nir + red + coefs[['Lsavi']])
    names(indx) <- "SAVI"

  } else if (index == "EVI") {

    if (missing(blue)) stop("nir, red and blue bands are needed.", call. = TRUE)

    indx <- coefs[['G']]*(nir - red)/(nir + coefs[['C1']]*red - coefs[['C2']]*blue + coefs[['Levi']])
    names(indx) <- "EVI"

  } else if (index == "NDWI") {

    indx <- (green - nir)/(green + nir)
    names(indx) <- "NDWI"

  } else if (index == "NBR") {

    indx <- (nir - swir)/(nir + swir)
    names(indx) <- "NBR"

  } else if (index == "NDSI") {

    indx <- (green - swir)/(green + swir)
    names(indx) <- "NDSI"

  } else if (index == "kNDVI") {

    sigma <- 0.5*(nir + red)
    var <- (nir - red)/(2*sigma)
    # Values
    vr <- getValues(var)
    # NA
    iNA <- which(!is.na(vr)) # NA positions
    # Get kNDVI
    indx <- raster(nir)
    indx[iNA] <- tanh(na.omit(vr))
    names(indx) <- "kNDVI"

  } else {
    stop("Index not supported.")
  }

  return(indx)
}

