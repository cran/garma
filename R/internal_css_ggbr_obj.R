#' Estimate a Ggbr model using a CSS (Conditional Sum-of-Squares) method.
#' internal_css_ggbr_obj - objective function to be minimised to get CSS estimates.
#' called from function "garma"
#' @param par - the parameters to evaluate the function at
#' @param params - other parameters - including the p, q, k, and scale parameters and (ss) the spectrum .
#' @return The value of the objective at the point par.
#' @noRd
internal_css_ggbr_obj <- function(par, params) {
  # Objective function to be minimised for CSS estimates
  y <- params$y
  p <- params$p
  q <- params$q
  k <- params$k
  include.mean <- params$include.mean

  beta0 <- 0
  start <- 1

  if (is.null(params$periods)) {
    # We are estimating u as well as fd
    u <- c()
    fd <- c()
    for (k1 in seq_len(k)) {
      u <- c(u, par[start])
      fd <- c(fd, par[start + 1])
      start <- start + 2
    }
  } else {
    # Fixed periods - do not estimate "u" it is a constant
    k <- length(params$periods)
    u <- cos(2 * pi / params$periods)
    fd <- par[start:(start + k - 1)]
    start <- start + k
  }

  y_dash <- y - beta0
  if (p > 0) phi_vec <- c(1, -(par[start:(start + p - 1)])) else phi_vec <- 1
  if (q > 0) theta_vec <- c(1, (par[(p + start):(length(par))])) else theta_vec <- 1

  arma_filter <- signal::Arma(a = theta_vec, b = phi_vec)
  eps <- signal::filter(arma_filter, y_dash)
  if (k > 0) {
    for (k1 in 1:k) {
      ggbr_filter <- signal::Arma(b = 1, a = internal_ggbr_coef(length(y_dash), fd[k1], u[k1]))
      eps <- signal::filter(ggbr_filter, eps)
    }
  }

  eps <- eps^2
  if (any(is.infinite(eps))) {
    ret <- 1.0e200
  } else {
    ret <- sum(eps, na.rm = TRUE)
  }

  return(ret)
}
