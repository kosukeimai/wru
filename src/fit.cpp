#include <RcppEigen.h>
#include "XName.h"
#incldue "keyWRU.h"

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppProgress)]]

using namespace Eigen;
using namespace Rcpp;
using namespace std;

//' Collapsed Gibbs sampler for keyWRU
//'
//' @param data A list with the following elements
//' @param ctrl A list of control arguments, including
//'
//' @keywords internal
// [[Rcpp::export]]
List keyWRU_fit(List data,
		List ctrl)
{
  keyWRU model(data, ctrl);
  model.sample();
  List res = model.return_obj();
  return res;
}

