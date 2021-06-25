#ifndef __aux_funs__
#define __aux_funs__

#include <RcppEigen.h>
inline int rand_wrapper(const int n); 
int rcat_without_normalize(Eigen::VectorXd& prob,
		       const double total,
		       const int size);

double mylgamma(const double x);


#endif
