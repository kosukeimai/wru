#include "sample_me.h"

//' Collapsed Gibbs sampler for hWRU. Internal function
//'
//' @param last_name Integer vector of last name identifiers for each record (zero indexed; as all that follow). Must match columns numbers in M_rs.  
//' @param first_name See last_name
//' @param mid_name See last_name
//' @param geo Integer vector of geographic units for each record. Must match column number in N_rg
//' @param N_rg Integer matrix of race | geography counts in census (geograpgies in columns).
//' @param pi_s Numeric matrix of race | surname prior probabilities.
//' @param pi_f Same as `pi_s`, but for first names.
//' @param pi_m Same as `pi_s`, but for middle names.
//' @param pi_nr Matrix of marginal probability distribution over missing names; non-keyword names default to this distribution.
//' @param which_names Integer; 0=surname only. 1=surname + first name. 2= surname, first, and middle names.
//' @param samples Integer number of samples to take after (in total)
//' @param burnin Integer number of samples to discard as burn-in of Markov chain
//' @param race_init Integer vector of initial race assignments
//' @param verbose Boolean; should informative messages be printed?
//'
//' @keywords internal
// [[Rcpp::export]]
arma::umat sample_me(const arma::uvec& last_name,
                     const arma::uvec& first_name,
                     const arma::uvec& mid_name,
                     const arma::uvec& geo,
                     const arma::umat& N_rg,
                     const arma::mat& pi_s,
                     const arma::mat& pi_f,
                     const arma::mat& pi_m,
                     const arma::mat& pi_nr,
                     const arma::uword which_names,
                     const arma::uword samples,
                     const arma::uword burnin,
                     const arma::uvec& race_init,
                     const bool verbose)
{ 
  arma::uword max_iter = samples + burnin;
  arma::uword N = last_name.n_elem; // Nr. of vf records 
  arma::uword J = pi_s.n_rows; // Nr. of racial cats
  arma::uword L = arma::max(geo) + 1; //Nr. of locations
  arma::uword S_KW = pi_s.n_cols; //Nr, of keyword surnames
  arma::uword F_KW, M_KW;
  if(which_names > 0){
    F_KW = pi_f.n_cols;
    if(which_names > 1){
      M_KW = pi_m.n_cols;
    }
  }
  arma::umat r_samp(N, J, arma::fill::zeros);
  arma::uvec race(&race_init[0], N);
  
  
  //Initialize global counts
  arma::umat n_rg(J, L, arma::fill::zeros);
  for(arma::uword i = 0; i < N; ++i) {
    n_rg(race_init[i], geo[i])++;
  }

  //Temporary storage
  arma::vec probs(J, arma::fill::zeros);
  arma::uword new_r = 0, r_i = 0, g_i = 0;;
  bool kw_lname = true, kw_fname = true, kw_mname = true; 
  for (arma::uword iter = 0; iter < max_iter; ++iter) {
    // Check stop signal
    Rcpp::checkUserInterrupt();
    // Sample from full conditionals
    for(arma::uword i = 0; i < N; ++i) {
      
      r_i = race[i];  g_i = geo[i];
      n_rg(r_i, g_i)--;
      kw_lname = last_name[i] < S_KW;
      if(which_names > 0){
        kw_fname = first_name[i] < F_KW;
        if(which_names > 1){
          kw_mname = mid_name[i] < M_KW;
        }
      }
      probs.zeros();
      for(arma::uword j = 0; j < J; ++j){
        // race | geo
        probs[j] += log(n_rg(j, g_i) + N_rg(j, g_i) + 1.0);
        // surname | race
        if(kw_lname){
          probs[j] += log(pi_s(j, last_name[i]) + 1e-8);
        } else {
          probs[j] += log(pi_nr(j, 0) + 1e-8);
        }
        //other names
        if(which_names > 0){
          if(kw_fname){
            probs[j] += log(pi_f(j, first_name[i]) + 1e-8);
          } else {
            probs[j] += log(pi_nr(j, 1) + 1e-8);
          }
          if(which_names > 1){
            if(kw_mname){
              probs[j] += log(pi_m(j, mid_name[i]) + 1e-8);
            } else {
              probs[j] += log(pi_nr(j, 2) + 1e-8);
            }
          }
        }
      }
      // exponentiate probabilities (using logsumexp)
      probs -= probs.max();
      probs = arma::exp(probs);
      probs /= arma::accu(probs);
      probs = arma::cumsum(probs);
      // //Sample race
      new_r = random_discrete(probs);
      race[i] = new_r;
      
      //Increment counts
      n_rg(new_r, g_i)++;
      //Store sample
      if(iter >= burnin){
        r_samp(i, new_r)++;
      }
    }
  }
  
  return(r_samp);
}