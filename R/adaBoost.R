#' Simulate from the Friedman model:
#'
#' [try to insert some latex here maybe]
#'
#' @param
#' @param
#' @param
#' @param
#' @export
#' @examples
#' sample(n)
#' # some more R code
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

    pred = as.integer(as.character(predict(tree, data.frame(X), type="class")))
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

#' Simulate from the Friedman model:
#'
#' [try to insert some latex here maybe]
#'
#' @param obj
#' @param new_data a matrix
#' @return
#' @export
#' @examples
#' sample(n)
#' # some more R code
predict.AdaBoost = function(obj, X, type="response"){
  f = 0
  for(i in seq_along(obj$alphas)){
    tree = obj$trees[[i]]
    tree$terms = obj$terms
    pred = as.integer(as.character(predict(tree, data.frame(X),
                                           type="class")))
    f = f + obj$alphas[i]*pred
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

#' Simulate from the Friedman model:
#'
#' [try to insert some latex here maybe]
#'
#' @param obj
#' @return
#' @export
#' @examples
#' sample(n)
#' # some more R code
print.AdaBoost = function(obj){
  cat('AdaBoost: tree_depth = ', obj$tree_depth, ' rounds = ',
      length(obj$alphas))
}



