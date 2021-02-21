#' Is a character, Raster or stars object?
#' @param x object to evaluate
#' @return a stars object
#' @noRd
ftb_whatkinditis <- function(x) {
  if (is.character(x)) {
    x <- stars::read_stars(x)
  } else if (inherits(x, "stars")) {
    x
  } else if (inherits(x, "RasterLayer") | inherits(x, "RasterStack") | inherits(x, "RasterBrick")) {
    x <- stars::st_as_stars(x)
    # stack(mapply(function(z) as(x[z],'Raster'),seq_len(length(x))))
  } else {
    stop(class(x), " class is not supported")
  }
  return(x)
}


#' Is a 3D or a 4D spatial object?
#' @importFrom methods is
#' @param x object to evaluate
#' @noRd
is_nD <- function(x) {
  if (is.character(x)) {
    xx <- stars::read_stars(x, proxy = TRUE)
    if (length(xx) > 1) time <- 1 else time <- 0
    nD <- sprintf("%sD", length(dim(xx)) + time)
  } else if (is(x, "stars")) {
    nD <- sprintf("%sD", length(dim(x)))
  } else if (is(x, "RasterStack") | is(x, "RasterBrick")) {
    nD <- "3D"
    # stack(mapply(function(z) as(x[z],'Raster'),seq_len(length(x))))
  } else {
    stop(class(x), " class is not supported")
  }
  return(nD)
}
