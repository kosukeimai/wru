#ifndef _HWRU
#define _HWRU

// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include "aux_funs.h"
// #include <progress.hpp>
// #include <progress_bar.hpp>

// #ifdef _OPENMP
// #include <omp.h>
// #endif
// 
// #pragma omp declare reduction( + : arma::uvec : omp_out += omp_in ) initializer( omp_priv = omp_orig )
// #pragma omp declare reduction( + : arma::umat : omp_out += omp_in ) initializer( omp_priv = omp_orig )
// #pragma omp declare reduction( + : arma::ucube : omp_out += omp_in ) initializer( omp_priv = omp_orig )

arma::umat hwru_sample(const arma::uvec& last_name,
                       const arma::uvec& first_name,
                       const arma::uvec& mid_name,
                       const arma::uvec& geo,
                       const arma::umat& N_rg,
                       const double& psi,
                       const arma::vec& alpha,
                       const arma::mat& pi_s,
                       const arma::mat& pi_f,
                       const arma::mat& pi_m,
                       const arma::mat& theta,
                       const arma::vec& lambda,
                       const arma::uword which_names,
                       const arma::uword samples,
                       const arma::uword burnin,
                       const bool me_,
                       const arma::uvec& race_init,
                       const bool verbose);



#endif