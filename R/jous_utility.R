
#' Return indices to be used for jittered data in oversampling
#'
#' @param ix_pos indices for positive examples in data
#' @param ix_neg indices for negative examples in data
#' @return returns a list, each of element of which gives indices to be used on
#'         a particular cut (note: will be of length delta - 1)
#' @export
index_over = function(ix_pos, ix_neg, q){

  if(length(ix_neg) == 0 | length(ix_pos) == 0 )
    stop("Must have at least one positive and one negative example!")

  if(!any(abs(q-0.5) < 1e-6))
    stop("Vector of quantiles must contain median!")

  ncut = length(q)

  ix_neg_cut = vector(mode="list", length=ncut)
  ix_pos_cut = vector(mode="list", length=ncut)

  for(i in 2:ncut){
    ix_neg_cut[[i]] = c(ix_neg_cut[[i-1]], sample(ix_neg, replace=T))
    ix_pos_cut[[ncut - i + 1]] = c(ix_pos_cut[[ncut - i + 2]],
                                   sample(ix_pos, replace=T))
  }

  # Keep the original data for the median classifier
  # gymnastics to assign NULL ...
  median_loc = which(q == 0.5)
  ix_neg_cut[median_loc] = list(NULL)
  ix_pos_cut[median_loc] = list(NULL)

  out = list(ix_neg_cut = ix_neg_cut, ix_pos_cut = ix_pos_cut)
  out
}

#' Return indices to be used in original data for undersampling
#'
#' (note: sampling is done without replacement)
#'
#' @param ix_pos indices for positive examples in data
#' @param ix_neg indices for negative examples in data
#' @return returns a list, each of element of which gives indices to be used on
#'         a particular cut (note: will be of length delta - 1)
#' @export
index_under = function(ix_pos, ix_neg, q, delta){

  if(length(ix_neg) == 0 | length(ix_pos) == 0 )
    stop("Must have at least one positive and one negative example!")

  if(!any(abs(q - 0.5) < 1e-6))
    stop("Vector of quantiles must contain median!")

  ncut = length(q)
  neach_pos = floor(1/delta * length(ix_pos))
  neach_neg = floor(1/delta * length(ix_neg))

  if(neach_pos == 0 | neach_neg == 0)
    stop("Must have at least floor(1/delta * n_sign) observations!")

  ix_neg_cut = vector(mode="list", length=ncut)
  ix_pos_cut = vector(mode="list", length=ncut)
  ix_neg_cut[[1]] = sample(ix_neg, size=neach_neg)
  ix_pos_cut[[ncut]] = sample(ix_pos, size=neach_pos)

  for(i in 2:ncut){

    ix_neg_cut[[i]] = c(ix_neg_cut[[i-1]],
                        sample(setdiff(ix_neg, ix_neg_cut[[i-1]]),
                               size=neach_neg))
    ix_pos_cut[[ncut - i + 1]] = c(ix_pos_cut[[ncut - i + 2]],
                                   sample(setdiff(ix_pos,
                                                  ix_pos_cut[[ncut - i + 2]]),
                                          size=neach_pos))
  }

  # Keep the original data for the median classifier
  median_loc = which(q == 0.5)
  ix_neg_cut[[median_loc]] = ix_neg
  ix_pos_cut[[median_loc]] = ix_pos

  out = list(ix_neg_cut = ix_neg_cut, ix_pos_cut = ix_pos_cut)
  out
}


#' @useDynLib JOUSBoost
#' @importFrom Rcpp sourceCpp
NULL

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




