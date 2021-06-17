#include "aux_funs.h"

inline int rand_wrapper(const int n) 
  {
  return floor(R::unif_rand() * n); 
  }


int rcat_without_normalize(Eigen::VectorXd &prob,
			   const double total,
			   const int size)
  { 
    // Draw from a categorical distribution
    // This function does not require a normalized probability vector.
    double u = R::unif_rand() * total;
    double temp = 0.0;
    int index = 0;
    for (int ii = 0; ii < size; ii++) {
      temp += prob(ii);
      if (u < temp) {
        index = ii;
        break;
      }
    }
    return index;
  }


double mylgamma(const double x){
    // gammaln_val = 0.0;
    // gammaln_val = lgamma(x);
    
    // Good approximation when x > 1
    //    x > 1: max abs err: 2.272e-03
    //    x > 0.5: 0.012
    //    x > 0.6: 0.008
    // Abramowitz and Stegun p.257
    
    if(x < 0.6)
      return (lgamma(x));
    else
      return ((x-0.5)*log(x) - x + 0.91893853320467 + 1/(12*x));
  };
