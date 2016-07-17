#' Jittering with Over/Under Sampling
#'
#' Perform probability estimation using jittering with over or undersampling
#'
#' @param X
#' @param y
#' @param delta
#' @param class_func function to perform classification.  This function must be
#'        exactly of the form class_func(x, y) where x is a matrix and y is a
#'        vector with entries in c(-1,1), and it must return an object on which
#'        pred_func can create predictions.  See examples.
#' @param pred_func function to create predictions.  This function must be
#'        exactly of the form pred_func(obj, x) where obj is an object returned
#'        by class_func and x is a matrix of new data values, and it must return
#'
#' @param type over or under
#' @param nu amount of jittering
#' @param X_pred a matrix of predictors for which to form probability estimates
#' @param keep_models whether to store all of the models used to create
#'        the probability estimates.  If False, the user will need to re-run.
#' @return JOUS object
#' @export
jous = function(X, y,
                class_func,
                pred_func,
                type="over",
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

  if(type == "over"){
    ix = index_over(ix_pos, ix_neg, q)
    col_stds = apply(X, 2, sd)
    X_jitter = sapply(1:ncol(X), function(i) X[,i] +
                        runif(n=nrow(X), -col_stds[i]*nu, col_stds[i]*nu))
    models = foreach(i = seq(ncuts), .inorder=T) %dopar% {
      ix_temp = c(ix$ix_neg_cut[[i]], ix$ix_pos_cut[[i]])
      class_func(rbind(X, X_jitter[ix_temp,]), c(y, y[ix_temp]))
    }
  } else if(type == "under"){
    ix = index_under(ix_pos, ix_neg, q, delta)
    models = foreach(i = seq(ncuts), .inorder=T) %dopar% {
      ix_temp = c(ix$ix_neg_cut[[i]], ix$ix_pos_cut[[i]])
      class_func(X[ix_temp,], y[ix_temp])
    }
  } else{
      stop("type must be either 'under' or 'over' ")
  }

  # create JOUS object
  jous_obj = list(delta=delta, nu=nu, q=q, type=type, models=models,
                  pred_func = pred_func)
  class(jous_obj) = "JOUS"

  # in sample
  jous_obj$phat_train = predict(jous_obj, X)

  # out of sample
  if(!is.null(X_pred))
    jous_obj$phat_test = predict(jous_obj, X_pred)

  if(!keep_models)
    jous_obj$models = NULL

  jous_obj

}

#' @export
predict.JOUS = function(object, X){

  if(is.null(object$models))
    stop("No saved models in your JOUS object.  Rerun with keep_models = T")

  delta = object$delta
  q = object$q

  # calculate predictions for each classifier
  pred_mat = sapply(object$models, function(z) object$pred_func(z, X))
  if(!all(pred_mat %in% c(-1,1)))
    stop("Your prediction function must return values only in -1, 1")

  median_loc = which(q == 0.5) - 1 # 0 based indexing here
  phat = grid_probs(pred_mat, q, delta, median_loc)
  phat

}

