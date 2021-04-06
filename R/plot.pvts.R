#' Plot of the "pvts" class
#'
#' @param x Object of class "pvts".
#' @param ylab Y-axis title.
#' @param type Line type.
#' @param col Line color.
#' @param lwd Line thickness.
#' @param main Title.
#' @param ... Additional graphical parameters to be passed to \link[base]{plot}.
#'
#' @importFrom grDevices adjustcolor
#'
#' @export
#'

plot.pvts <- function(x, ylab, type, col, lwd, main, ...) {
  ylabel <- missing(ylab)
  title <- missing(main)
  wl <- missing(lwd)
  clr <- missing(col)
  tl <- missing(type)

  p <- x$Monitoring_period[2]
  li <- x$Threshold[2]
  m <- x$Ts[1]

  if (is(x$Ts, "ts")) {
    m <- time(x$Ts)[1]
  }

  if (!is.na(x$Breakpoint[1])) {
    label <- "Breakpoint Detected: Non-seasonal detection approach"
  } else {
    label <- "Breakpoint Not Detected"
  }

  if (ylabel) {
    ylab <- "Variable"
  }

  if (title) {
    main <- label
  }

  if (wl) {
    lwd <- 1.5
  }

  if (clr) {
    col <- "gray45"
  }

  if (tl) {
    type <- "l"
  }

  plot(x$Ts, ylab = ylab, type = type, lwd = lwd, col = col, main = main, ...)
  grid()
  graphics::lines(x$Ts, type = "l", col = "gray45", lwd = 1.5)
  points(x$Ts, pch = 19, lwd = 2, cex = 1.1)
  abline(h = x$Threshold[2], col = "red", lty = 2, lwd = 2)

  if (!is.na(x$Breakpoint[1])) {
    abline(v = c(p - 1 / 2, p + 1 / 2), col = "blue", lty = 3, lwd = 2)
    x1 <- c(p - 1 / 2, p - 1 / 2, p + 1 / 2, p + 1 / 2)
    y1 <- c(-1e+04, max(x$Ts) + 1e+04, max(x$Ts) + 1e+04, -1e+04)

    polygon(x1, y1, col = adjustcolor("slateblue1",alpha.f = 0.4),border = NA)

    legend(m, max(x$Ts)*0.65, c("Ts","Lower limit","Breakpoint detected"),inset = .02, cex = 1, lty = c(1,2,1), lwd = c(2,2,5), col = c("black","red", "slateblue1"), bty = "n", y.intersp = 0.8, x.intersp = 0.2)

  } else {
    abline(v = c(p - 1 / 2, p + 1 / 2), col = "royalblue3", lty = 3, lwd = 0.8)

    legend(m, max(x$Ts)*0.65, c("Ts","Lower limit","Breakpoint Not detected"),inset = .02, cex = 1, lty = c(1,2,1), lwd = c(2,2,5), col = c("black","red", "slateblue1"), bty = "n", y.intersp = 1, x.intersp = 0.2)
  }
}
