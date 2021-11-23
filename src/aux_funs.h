#ifndef _AUX
#define _AUX

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]


#include <RcppArmadillo.h>
arma::uword random_discrete(arma::vec& breaks);

double mylgamma(const double x);


#endif
