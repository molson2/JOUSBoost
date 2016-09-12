#' Jittering with Over/Under Sampling
#'
#' Perform probability estimation using jittering with over or undersampling.
#'
#' @param X A matrix of continuous predictors.
#' @param y A vector of responses with entries in \code{c(-1, 1)}.
#' @param delta An integer (greater than 3) to control the number of quantiles to
#'        estimate:
#' @param class_func Function to perform classification.  This function must be
#'        exactly of the form \code{class_func(X, y)} where X is a matrix and y is a
#'        vector with entries in \code{c(-1, 1)}, and it must return an object on which
#'        \code{pred_func} can create predictions.  See examples.
#' @param pred_func Function to create predictions.  This function must be
#'        exactly of the form \code{pred_func(fit_obj, X)} where \code{fit_obj}
#'        is an object returned by class_func and X is a matrix of new data
#'        values, and it must return a vector with entries in \code{c(-1, 1)}.
#'        See examples.
#' @param type Type of sampling: "over" for oversampling,  or "under" for
#'        undersampling.
#' @param nu The amount of noise to apply to predictors when oversampling data.
#'        The noise level is controlled by \code{nu * sd(X[,j])} for each
#'        predictor - the default of \code{nu = 1} works well.  Such "jittering"
#'        of the predictors is essential when applying \code{jous} to boosting
#'        type methods.
#' @param X_pred A matrix of predictors for which to form probability estimates.
#' @param keep_models Whether to store all of the models used to create
#'        the probability estimates.  If \code{type=FALSE}, the user will need
#'        to re-run \code{jous} when creating probability estimates for test data.
#'
#' @return Returns an object of class JOUS containing information about the
#' parameters used in the \code{jous} function call, as well as the following
#' additional components:
#' \item{q}{The vector of target quantiles estimated by \code{jous}.  Note that
#' the estimated probabilities will be located at the midpoints of the values in
#' \code{q}.}
#' \item{phat_train}{The in-sample probability estimates \eqn{p(y=1|x)}.}
#' \item{phat_test}{Probability estimates for the optional test data in \code{X_test}}
#' \item{models}{If \code{keep_models=TRUE}, a list of models fitted to
#' the resampled data sets.}
#'
#' @note The \code{jous} function runs the classifier \code{class_func} a total
#' of \code{delta} times on the data, which can be computationally expensive.
#' Also,\code{jous} cannot yet be applied to categorical predictors - in the
#' oversampling case, it is not clear how to "jitter" a categorical variable.
#'
#' @references Mease, D., Wyner, A. and Buja, A. (2007). Costweighted
#' boosting with jittering and over/under-sampling:
#' JOUS-boost. J. Machine Learning Research 8 409-439.
#'
#' @examples
#' \dontrun{
#' # Generate data from Friedman model #
#' set.seed(111)
#' dat = friedman_data(n = 500, d = 10, gamma = 0.5)
#' train_index = sample(1:500, 400)
#'
#' # Apply jous to adaBoost classifier
#' class_func = function(X, y) adaBoost(X, y, tree_depth = 2, n_rounds = 100)
#' pred_func = function(fit_obj, X_test) predict(fit_obj, X_test)
#'
#' jous_fit = jous(dat$X[train_index,], dat$y[train_index], class_func,
#'                 pred_func, type="under", delta=10, keep_models=TRUE)
#' # get probability
#' phat_jous = predict(jous_fit, dat$X[-train_index, ], type="prob")
#'
#' # compare with probability from adaBoost
#' ada = adaBoost(dat$X[train_index,], dat$y[train_index], tree_depth = 2,
#'                n_rounds = 100)
#' phat_ada = predict(ada, dat$X[train_index,], type="prob")
#'
#' mean((phat_jous - dat$p[-train_index])^2)
#' mean((phat_ada - dat$p[-train_index])^2)
#' }
#'
#' @export
jous = function(X, y,
                class_func,
                pred_func,
                type="under",
                delta = 10,
                nu=1,
                X_pred=NULL,
                keep_models=F){

  # check data types
  if(!all(y %in% c(-1,1)))
    stop("y must take values in -1, 1")

  if(!is.matrix(X))
    stop("X must be a matrix")

  if(delta < 3)
    stop("delta must be an integer greater than 2")

  ix_pos = which(y == 1)
  ix_neg = which(y == -1)
  ncuts = delta - 1

  q = (1:ncuts)/delta
  # if the 0.5 isn't in q, insert it
  if(!any(q == 0.5)){
    q <- c(q[q < 0.5], 0.5, q[q > 0.5])
    ncuts = ncuts + 1
  }

  # Fit models over tilted data
  models = list()
  if(type == "over"){
    ix = index_over(ix_pos, ix_neg, q)
    col_stds = apply(X, 2, stats::sd, na.rm=TRUE)
    X_jitter = sapply(1:ncol(X), function(i) X[,i] +
                        stats::runif(n=nrow(X), -col_stds[i]*nu, col_stds[i]*nu))
    for(i in seq(ncuts)){
      ix_temp = c(ix$ix_neg_cut[[i]], ix$ix_pos_cut[[i]])
      models[[i]] = class_func(rbind(X, X_jitter[ix_temp,]), c(y, y[ix_temp]))
    }
  } else if(type == "under"){
    ix = index_under(ix_pos, ix_neg, q, delta)
    for(i in seq(ncuts)){
      ix_temp = c(ix$ix_neg_cut[[i]], ix$ix_pos_cut[[i]])
      models[[i]] = class_func(X[ix_temp,], y[ix_temp])
    }
  } else{
      stop("type must be either 'under' or 'over' ")
  }

  # create JOUS object
  jous_obj = list(delta=delta, nu=nu, q=q, type=type, models=models,
                  pred_func = pred_func)
  class(jous_obj) = "JOUS"

  # in sample
  jous_obj$phat_train = stats::predict(jous_obj, X, type="prob")

  # out of sample
  if(!is.null(X_pred))
    jous_obj$phat_test = stats::predict(jous_obj, X_pred, type="prob")

  if(!keep_models)
    jous_obj$models = NULL

  jous_obj

}

#' Create predictions
#'
#' Makes a prediction on new data for a given fitted \code{jous} model.
#' @param object An object of class JOUS returned by the \code{jous} function.
#' @param X A design matrix of predictors.
#' @param type The type of prediction to return.  If \code{type="response"}, a
#'        class label of -1 or 1 is returned.  If \code{type="prob"}, the
#'        probability \eqn{p(y=1|x)} is returned.
#' @param ... \dots
#'
#' @return Returns a vector of class predictions if \code{type="response"}, or a
#'          vector of class probabilities \eqn{p(y=1|x)} if \code{type="prob"}.
#'
#' @examples
#' \dontrun{
#' #' # Generate data from Friedman model #
#' set.seed(111)
#' dat = friedman_data(n = 500, d = 10, gamma = 0.5)
#' train_index = sample(1:500, 400)
#'
#' # Apply jous to adaBoost classifier
#' class_func = function(X, y) adaBoost(X, y, tree_depth = 2, n_rounds = 100)
#' pred_func = function(fit_obj, X_test) predict(fit_obj, X_test)
#'
#' jous_fit = jous(dat$X[train_index,], dat$y[train_index], class_func,
#'                 pred_func, type="under", delta=10, keep_models=TRUE)
#' # get class prediction
#' yhat = predict(jous_fit, dat$X[-train_index, ])
#' # get probability estimate
#' phat = predict(jous_fit, dat$X[-train_index, ], type="prob")
#' }
#' @export predict.JOUS
#' @export
predict.JOUS = function(object, X, type="response", ...){

  if(is.null(object$models))
    stop("No saved models in your JOUS object.  Rerun with keep_models = TRUE")

  delta = object$delta
  q = object$q

  # calculate predictions for each classifier
  pred_mat = sapply(object$models, function(z) object$pred_func(z, X))
  if(!all(pred_mat %in% c(-1,1)))
    stop("Your prediction function must return values only in -1, 1")

  median_loc = which(q == 0.5) - 1 # 0 based indexing here
  phat = grid_probs(pred_mat, q, delta, median_loc)

  # handle response type
  if(type == "response"){
    2*(phat > 0.5) - 1
  } else if(type =="prob"){
    phat
  } else {
    stop('type must be either "response" or "prob"')
  }

}

