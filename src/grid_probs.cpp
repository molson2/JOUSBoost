#include <Rcpp.h>
using namespace Rcpp;
#define TOL 1e-8

//' @export
// [[Rcpp::export]]
NumericVector grid_probs(IntegerMatrix X, NumericVector q, double delta,
                       int median_loc) {

  // Check to make sure the median is located at median_loc
  if(fabs(q[median_loc] - 0.5) > TOL)
    stop("Check your median location!");
  int n_pred = X.nrow(), i, j;
  NumericVector phat(n_pred);

  for(i = 0; i < n_pred; i++){

    if(X[i,median_loc] == 1){
      phat[i] = 1 - 1/(2*delta);
      for(j = median_loc + 1; j < n_pred; j++)
        if(X[i,j] == -1){
          phat[i] = q[j] - 1/(2*delta);
          break;
        }
      } else{
        phat[i] = 1/(2*delta);
        for(j = median_loc - 1; j >= 0; j--)
          if(X[i,j] == 1){
            phat[i] = q[j] + 1/(2*delta);
            break;
          }
      }
  }

  return phat;

}

