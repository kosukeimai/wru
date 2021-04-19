#include <RcppEigen.h>
#include "XName.h"
#include "keyWRU.h"

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppProgress)]]

//' Collapsed Gibbs sampler for keyWRU
//'
//' @param data A list with the following elements
//' @param ctrl A list of control arguments, including
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List keyWRU_fit(Rcpp::List data,
                      Rcpp::List ctrl)
{
  keyWRU model(data, ctrl);
  model.sample();
  Rcpp::List res = model.return_obj();
  return res;
}

