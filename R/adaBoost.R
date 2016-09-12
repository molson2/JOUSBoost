#' AdaBoost Classifier
#'
#' An implementation of the adaBoost algorithm from Freund and Shapire (1997)
#' applied to decision tree classifiers.
#'
#' @param X A matrix of continuous predictors.
#' @param y A vector of responses with entries in \code{c(-1, 1)}.
#' @param tree_depth The depth of the base tree classifier to use.
#' @param n_rounds The number of rounds of boosting to use.
#' @param verbose Whether to print the number of iterations.
#'
#' @references Freund, Y. and Schapire, R. (1997). A decision-theoretic
#' generalization of online learning and an application to boosting, Journal of
#'  Computer and System Sciences 55: 119-139.
#'
#' @return Returns an object of class AdaBoost containing the following values:
#' \item{alphas}{Weights computed in the adaBoost fit.}
#' \item{trees}{The trees constructed in each round of boosting.  Storing trees
#'              allows one to make predictions on new data.}
#'
#' @note Trees are grown using the CART algorithm implemented in the \code{rpart}
#'       package.  In order to conserve memory, the only parts of the fitted
#'       tree objects that are retained are those essential to making predictions.
#'       In practice, the number of rounds of boosting to use is chosen by
#'       cross-validation.
#'
#' @examples
#' \dontrun{
#' # Generate data from the circle model
#' set.seed(111)
#' dat = circle_data(n = 500)
#' train_index = sample(1:500, 400)
#'
#' ada = adaBoost(dat$X[train_index,], dat$y[train_index], tree_depth = 2,
#'                n_rounds = 200, verbose = TRUE)
#' print(ada)
#' yhat_ada = predict(ada, dat$X[-train_index,])
#'
#' # calculate misclassification rate
#' mean(dat$y[-train_index] != yhat_ada)
#' }
#' @export
adaBoost = function(X, y, tree_depth = 3, n_rounds = 100, verbose = FALSE){

  # check data types
  if(!all(y %in% c(-1,1)))
    stop("y must take values in -1, 1")

  if(!is.matrix(X))
    stop("X must be a matrix")

  control = rpart::rpart.control(minsplit = 0, minbucket = 1, cp = -1,
                                 maxcompete = 0, maxsurrogate = 0,
                                 usesurrogate = 0, xval = 0,
                                 maxdepth = tree_depth)

  n = dim(X)[1]
  w = rep(1/n, n)
  trees = list()
  alphas = list()

  for(i in seq(n_rounds)){

    tree = rpart::rpart(y ~ ., data = data.frame(X), weights = w,
                        method = "class", control = control, x=FALSE, y=FALSE,
                        model=FALSE)
    # trim tree object
    tree$where=NULL
    tree$call=NULL
    tree$cptable=NULL
    tree$functions=NULL
    tree$control=NULL
    tree$variable.importance=NULL
    tree$parms=NULL

    pred = as.integer(as.character(stats::predict(tree, data.frame(X), type="class")))
    e = sum(w*(pred != y))

    # If tree perfectly gets data, boosting terminates
    if(abs(e) < 1e-8)
      break

    alpha = 1/2*log((1-e)/e)
    w = w*exp(-alpha*pred*y)
    w = w/sum(w)

    # kill formulas since they waste memory
    if(i == 1){
      terms = tree$terms
    } else{
      tree$terms = NULL
    }

    trees[[i]] = tree
    alphas[[i]] = alpha

    if(verbose & (i %% 10 == 0))
      cat("Iteration: ", i, "\n")

  }

  out = list(alphas = unlist(alphas), trees = trees, tree_depth = tree_depth,
             terms=terms)
  class(out) = "AdaBoost"
  out

}

#' Create predictions from AdaBoost fit
#'
#' Makes a prediction on new data for a given fitted \code{adaBoost} model.
#'
#' @param object An object of class AdaBoost returned by the \code{adaBoost} function.
#' @param X A design matrix of predictors.
#' @param type The type of prediction to return.  If \code{type="response"}, a
#'        class label of -1 or 1 is returned.  If \code{type="prob"}, the
#'        probability \eqn{p(y = 1 | x)} is returned.
#' @param ... \dots
#'
#' @return Returns a vector of class predictions if \code{type="response"}, or a
#'          vector of class probabilities \eqn{p(y=1|x)} if \code{type="prob"}.
#'
#' @note Probabilities are estimated according to the formula:
#'       \deqn{p(y=1| x) = 1/(1 + exp(-2*f(x)))}
#'       where \eqn{f(x)} is the score function produced by adaBoost.  See
#'       Friedman (2000).
#'
#' @references Friedman, J., Hastie, T. and Tibshirani, R. (2000). Additive logistic
#' regression: a statistical view of boosting (with discussion), Annals of
#' Statistics 28: 337-307.
#'
#' @examples
#' \dontrun{
#' # Generate data from the circle model
#' set.seed(111)
#' dat = circle_data(n = 500)
#' train_index = sample(1:500, 400)
#'
#' ada = adaBoost(dat$X[train_index,], dat$y[train_index], tree_depth = 2,
#'                n_rounds = 100, verbose = TRUE)
#' # get class prediction
#' yhat = predict(ada, dat$X[-train_index, ])
#' # get probability estimate
#' phat = predict(ada, dat$X[-train_index, ], type="prob")
#' }
#' @export predict.AdaBoost
#' @export
predict.AdaBoost = function(object, X, type="response", ...){
  f = 0
  for(i in seq_along(object$alphas)){
    tree = object$trees[[i]]
    tree$terms = object$terms
    pred = as.integer(as.character(stats::predict(tree, data.frame(X),
                                           type="class")))
    f = f + object$alphas[i]*pred
  }

  # handle response type
  if(type == "response"){
    sign(f)
  } else if(type =="prob"){
    1/(1+exp(-2*f))
  } else {
    stop('type must be either "response" or "prob"')
  }
}

#' Print a summary of adaBoost fit.
#' @param x An AdaBoost object fit using the \code{adaBoost} function.
#' @param ... \dots
#' @return Printed summary of the fit, including information about the tree
#'         depth and number of boosting rounds used.
#' @export
print.AdaBoost = function(x, ...){
  cat('AdaBoost: tree_depth = ', x$tree_depth, ' rounds = ',
      length(x$alphas), '\n')
}



