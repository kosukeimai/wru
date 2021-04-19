#ifndef __aux__
#define __aux__

#include <RcppEigen.h>
int rand_wrapper(const int n); 
std::vector<int> shuffle_indeces(const int m);
int rcat_without_normalize(Eigen::VectorXd& prob,
		       const double total,
		       const int size);

bool found_in(Rcpp::IntegerVector x, int y);
double mylgamma(const double x);


#endif
