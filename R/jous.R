
#' @export
jous = function(x, y, delta,
                class_func,
                pred_func,
                type="over",
                nu=1,
                x_pred = NULL,
                keep_models=F) {

  # check data types
  if(!all(y %in% c(-1,1)))
    stop("y must take values in -1, 1")

  ix_pos = which(y == 1)
  ix_neg = which(y == -1)
  ncuts = delta - 1
  models = list()

  q = 1:ncuts/delta
  # if the 0.5 isn't in q, insert it
  if(!any(q == 0.5))
    q <- c(q[q < 0.5], 0.5, q[q > 0.5])

  if(type == "over"){

  }else{

  }


  jous_obj = list(delta=delta, nu=nu, type=type, models=NULL,
                  pred_func = pred_func, q = q)
  class(jous_obj) = "JOUS"
  jous_obj
}

