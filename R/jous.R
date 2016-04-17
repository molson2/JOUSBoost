#' Jittering with Over/Under Sampling
#'
#' Perform probability estimation using jittering with over or undersampling
#'
#' @param x
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
#' @param type
#' @param nu
#' @param x_pred
#' @param keep_models
#' @return JOUS object
#' @export
jous = function(x, y,
                class_func,
                pred_func,
                type="over",
                delta = 10,
                nu=1,
                x_pred = NULL,
                keep_models=F){

  # check data types
  if(!all(y %in% c(-1,1)))
    stop("y must take values in -1, 1")

  if(!is.matrix(x))
    stop("x must be a matrix")

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
    col_stds = apply(x, 2, sd)
    x_jitter = sapply(1:ncol(x), function(i) x[,i] +
                        runif(n=nrow(x), -col_stds[i]*nu, col_stds[i]*nu))
    models = foreach(i = seq(ncuts), .inorder=T) %dopar% {
      ix_temp = c(ix$ix_neg_cut[[i]], ix$ix_pos_cut[[i]])
      class_func(rbind(x, x_jitter[ix_temp,]), c(y, y[ix_temp]))
    }
  }else{
    ix = index_under(ix_pos, ix_neg, q, delta)
    models = foreach(i = seq(ncuts), .inorder=T) %dopar% {
      ix_temp = c(ix$ix_neg_cut[[i]], ix$ix_pos_cut[[i]])
      class_func(x[ix_temp,], y[ix_temp])
     }
  }

  # create JOUS object
  jous_obj = list(delta=delta, nu=nu, q=q, type=type, models=models,
                  pred_func = pred_func)
  class(jous_obj) = "JOUS"

  # in sample
  jous_obj$phat_train = predict(jous_obj, x)

  # out of sample
  if(!is.null(x_pred))
    jous_obj$phat_test = predict(jous_obj, x_pred)

  if(!keep_models)
    jous_obj$models = NULL

  jous_obj

}

