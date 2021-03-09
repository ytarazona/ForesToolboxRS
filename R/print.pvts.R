#' Printing of the "pvts" class
#'
#' @param x Object of class "pvts".
#' @param ... Not used
#'
#' @param x Object of class "pvts".
#'
#' @export
#'

print.pvts <- function(x) {
  cat("\n  NON-SEASONAL DETECTION APPROACH
        PVts-Beta Approach\n")
  if (!is.na(x$Breakpoint[1])) {
    cat("\nBreakpoint Detected \n")
    print(x$Breakpoint)
    cat("\nMonitoring Period \n")
    print(x$Monitoring_period)
  } else {
    cat("\nBreakpoint Not Detected\n")
    print(cd$Breakpoint)
    cat("\nMonitoring Period \n")
    print(x$Monitoring_period)
  }
  cat("\nDetection limit \n")
  print(x$Threshold)
}
