#ifndef _HWRUME
#define _HWRUME

// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include "aux_funs.h"

arma::umat sample_me(const arma::uvec& last_name,
                     const arma::uvec& first_name,
                     const arma::uvec& mid_name,
                     const arma::uvec& geo,
                     const arma::umat& N_rg,
                     const arma::mat& alpha,
                     const arma::mat& beta_s,
                     const arma::mat& beta_f,
                     const arma::mat& beta_m,
                     const arma::vec& pi_nr,
                     const arma::uword which_names,
                     const arma::uword samples,
                     const arma::uword burnin,
                     const bool me_race,
                     const arma::uvec& race_init,
                     const bool verbose);


#endif