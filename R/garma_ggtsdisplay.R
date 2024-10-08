#' ggtsdisplay of underlying ARMA process.
#'
#' For a Gegenbauer process, use semi-parametric methods to obtain short memory version of the process, then run a ggtsdisplay().
#'
#' The purpose of this function is to ease the process of identifying the underlying short memory process.
#'
#' @param x (num) This should be a numeric vector representing the process to estimate.
#' @param k (int) The number of Gegenbauer factors
#' @param ... additional parameters to pass to ggtsdisplay
#' @return A ggplot object.
#' @examples
#' data(AirPassengers)
#' ap <- as.numeric(diff(AirPassengers, 12))
#' garma_ggtsdisplay(ap)
#' @export
garma_ggtsdisplay <- function(x, k = 1, ...) {
  sp <- ggbr_semipara(x, k = k)
  arma_process <- extract_arma(x, sp)
  ggtsdisplay(arma_process, ...)
}
