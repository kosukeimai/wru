#include "aux_funs.h"

// based on scalar-input version of "findInterval2", which is
// available at http://adv-r.had.co.nz/Rcpp.html
arma::uword random_discrete(arma::vec& breaks)
{
  
  double u = R::runif(0.0, 1.0);
  arma::uword out;
  
  arma::vec::iterator pos;
  pos = std::upper_bound(breaks.begin(), breaks.end(), u);
  out = std::distance(breaks.begin(), pos);
  
  return out;
}



double mylgamma(const double x){
  if(x < 0.6)
    return (lgamma(x));
  else
    return ((x-0.5)*log(x) - x + 0.91893853320467 + 1/(12*x));
}

