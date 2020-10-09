#' Greenness Vegetation Index (GEVI)
#'
#' Greenness Vegetation Index is obtained from the Tasseled Cap Transformation.
#'
#' @author Yonatan Tarazona
#'
#' @section Details:
#' Currently implemented for satellites such as Landsat-4 TM, Landsat-5 TM, Landsat-7 ETM+,
#' Landsat-8 OLI and Sentinel2. The input data must be in top of atmosphere reflectance (toa).
#' Bands required as input must be ordered as:
#'
#' \tabular{ll}{
#' Type of Sensor \tab Name of bands \cr
#' Landsat4TM  \tab blue,green,red,nir,swir1,swir2  \cr
#' Landsat5TM  \tab blue,green,red,nir,swir1,swir2 \cr
#' Landsat7ETM+ \tab blue,green,red,nir,swir1,swir2 \cr
#' Landsat8OLI  \tab blue,green,red,nir,swir1,swir2 \cr
#' Landsat8OLI-Li2016  \tab coastal,blue,green,red,nir,swir1,swir2 \cr
#' Sentinel2MSI \tab coastal,blue,green,red,nir-1,mir-1,mir-2 \cr
#' }
#'
#' @param x It could be RasterStack or RasterBrick.
#' @param sat Specify satellite and sensor type (Landsat5TM, Landsat7ETM or Landsat8OLI).
#'
#' @examples
#' library(ForesToolboxRS)
#' library(raster)
#'
#' # Load an example dataset
#' data(FTdata)
#'
#' # Tasseled-cap using Landsat8OLI
#' tasscap <- tct(x = img, sat = "Landsat8OLI")
#'
#' @export
#'
gevi <- function(x, sat = "Landsat8OLI"){

  if (sat == "Landsat4TM"){
    coefc <- matrix(c(0.3037,  0.2793,  0.4743, 0.5585, 0.5082,   0.1863,
                      -0.2848, -0.2435, -0.5436, 0.7243, 0.0840,  -0.1800,
                      0.1509,  0.1973,  0.3279, 0.3406, -0.7112, -0.4572),3, 6,
                    byrow = TRUE, dimnames = list(c("Brightness","Greenness","Wetness"),
                                                 c("B1","B2","B3","B4","B5","B7")))
  } else if (sat == "Landsat5TM") {
    coefc <- matrix(c(0.2909,  0.2493,  0.4806, 0.5568, 0.4438,   0.1706,
                      -0.2728, -0.2174, -0.5508, 0.7221, 0.0733,  -0.1648,
                      0.1446,  0.1761,  0.3322, 0.3396, -0.6210, -0.4186),3, 6,
                    byrow = TRUE, dimnames = list(c("Brightness","Greenness","Wetness"),
                                                 c("B1","B2","B3","B4","B5","B7")))
  } else if (sat == "Landsat7ETM") {
    coefc <- matrix(c(0.3561,  0.3972,  0.3904, 0.6966,  0.2286,  0.1596,
                      -0.3344, -0.3544, -0.4556, 0.6966, -0.0242, -0.2630,
                      0.2626,  0.2141,  0.0926, 0.0656, -0.7629,  0.5388),3, 6,
                    byrow = TRUE, dimnames = list(c("Brightness","Greenness","Wetness"),
                                                 c("B1","B2","B3","B4","B5","B7")))
  } else if (sat == "Landsat8OLI") {
    coefc <- matrix(c(0.3029,  0.2786,  0.4733, 0.5599,  0.5080,  0.1872,
                      -0.2941, -0.2430, -0.5424, 0.7276,  0.0713, -0.1608,
                      0.1511,  0.1973,  0.3283, 0.3407, -0.7117, -0.4559),3, 6,
                    byrow = TRUE, dimnames = list(c("Brightness","Greenness","Wetness"),
                                                 c("B1","B2","B3","B4","B5","B7")))
  } else if (sat == "Landsat8OLI-Li2016") {
    coefc <- matrix(c(0.2540,  0.3037,  0.3608,  0.3564, 0.7084,  0.2358,  0.1691,
                      -0.2578, -0.3064, -0.3300, -0.4325, 0.6860, -0.0383, -0.2674,
                      0.1877,  0.2097,  0.2038,  0.1017, 0.0685, -0.7460, -0.5548),3, 7,
                    byrow = TRUE, dimnames = list(c("Brightness","Greenness","Wetness"),
                                                 c("B1","B2","B3","B4","B5","B6","B7")))
  } else if (sat == "Sentinel2MSI") {
    coefc <- matrix(c(0.2381,  0.2569,  0.2934,  0.3020, 0.3580,  0.0896,  0.0780,
                      -0.2266, -0.2818, -0.3020, -0.4283, 0.3138, -0.1341, -0.2538,
                      0.1825,  0.1763,  0.1615,  0.0486, -0.0755,-0.7701, -0.5293),3, 7,
                    byrow = TRUE, dimnames = list(c("Brightness","Greenness","Wetness"),
                                                 c("B1","B2","B3","B4","B8","B11","B12")))
  } else stop("Satellite not supported.", call. = TRUE)

  val <- as.matrix(x)%*%t(coefc)

  bgw <- img[[1:3]]

  for (i in 1:3) {
    values(bgw[[i]]) <- val[,i]
  }

  gevi_index <- bgw[[2]]/(bgw[[1]] + bgw[[2]] + bgw[[3]])

  return(gevi_index)
}
