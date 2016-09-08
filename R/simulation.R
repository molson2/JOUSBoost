# -----------------------------------------------------------------------------
#                            Functions to Simulate Data
# -----------------------------------------------------------------------------

#' Simulate from the circle model:
#'
#' [try to insert some latex here maybe]
#'
#' @param n
#' @param inner_r
#' @param outer_r
#' @return
#' @export
circle_data = function(n = 500, inner_r = 8, outer_r = 28){
  if(outer_r <= inner_r)
    stop('outer_r must be strictly larger than inner_r')

  X = matrix(runif(2*n, 0, outer_r), nrow=n, ncol=2)
  r = apply(X, 1, function(x) sqrt(sum(x^2)))
  p = 1*(r < inner_r) +
    (outer_r-r)/(outer_r-inner_r)*((inner_r < r) & (r < outer_r))
  y = 2*rbinom(n, 1, p) - 1
  list(X=X, y=y, p=p)
}

#' Simulate from the Friedman model:
#'
#' [try to insert some latex here maybe]
#'
#' @param n
#' @param
#' @param
#' @return
#' @export
friedman_data = function(n = 500, d = 10, gamma = 10){
  if(d < 6)
    stop('d must be greater than 6')
  X = matrix(rnorm(d*n), nrow=n, ncol=d)
  log_odds = gamma*(1 - X[,1] + X[,2] - X[,3] + X[,4] - X[,5] + X[,6])*
    rowSums(X[,1:6])
  p = exp(log_odds)/(1 + exp(log_odds))
  y = 2*rbinom(n, 1, p) - 1
  list(X=X, y=y, p=p)

}
