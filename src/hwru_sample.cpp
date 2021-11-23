#include "hwru_sample.h"

//' Collapsed Gibbs sampler for hWRU. Internal function
//'
//' @param last_name Integer vector of last name identifiers for each record.
//' @param first_name See last_name
//' @param middle_name See last_name
//'
//' @keywords internal
// [[Rcpp::export]]
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
                       const bool verbose)
{ 
  arma::uword max_iter = samples + burnin;
  arma::uword N = last_name.size();
  arma::uword K = pi_s.n_rows;
  arma::uword S = arma::max(last_name) + 1;
  arma::uword L = arma::max(geo) + 1;
  arma::uword S_KW = pi_s.n_cols;
  arma::uword F_KW, M_KW;
  if(which_names > 0){
    F_KW = pi_f.n_cols;
    if(which_names > 1){
      M_KW = pi_m.n_cols;
    }
  }
  arma::umat r_samp(N, K, arma::fill::zeros);
  arma::uvec race(&race_init[0], N);
  
  //Temporary storage
  arma::uvec n_r(K, arma::fill::zeros);
  arma::umat n_rs(K, S, arma::fill::zeros);
  arma::umat m_rg(K, L, arma::fill::zeros);
  arma::umat n_rf, n_rm;
  if(which_names > 0){
    n_rf = n_rf.zeros(K, arma::max(first_name) + 1);
    if(which_names > 1){
      n_rm = n_rm.zeros(K, arma::max(mid_name) + 1);
    }
  }
  
  //Initialize global counts
  arma::uword r_i = 0, l_i = 0, g_i = 0;
  for(arma::uword i = 0; i < N; ++i) {
    r_i = race_init[i];
    n_r(r_i)++;
    n_rs(r_i, last_name[i])++;
    m_rg(r_i, geo[i])++;
    if(which_names > 0){
      n_rf(r_i, first_name[i])++;
      if(which_names > 1){
        n_rm(r_i, mid_name[i])++;
      }
    }
  }
  const arma::urowvec N_g = arma::sum(N_rg);
  const double phi = arma::accu(N_rg);
  arma::urowvec m_g = arma::sum(m_rg);
  
  arma::vec probs(K, arma::fill::zeros);
  //Progress progress_bar(max_iter, verbose);
  arma::uword new_r = 0;
  bool kw_lname = true, kw_fname = true, kw_mname = true; 
  for (int iter = 0; iter < max_iter; ++iter) {
      // Check stop signal
      Rcpp::checkUserInterrupt();
    // Sample from full conditionals
    for(arma::uword i = 0; i < N; ++i) {
      r_i = race[i]; l_i = last_name[i]; g_i = geo[i];
      kw_lname = l_i < S_KW;
      if(which_names > 0){
        kw_fname = first_name[i] < F_KW;
        if(which_names > 1){
          kw_mname = mid_name[i] < M_KW;
        }
      }
      n_r(r_i)--;
      n_rs(r_i, l_i)--;
      m_rg(r_i, g_i)--;
      probs.zeros();
      for(arma::uword k = 0; k < K; ++k){
        probs[k] = 
         (kw_lname ? log(n_rs(k, l_i) + psi * pi_s(k, l_i) + 1e-8) - log(n_rs(k, l_i) + psi + 1e-8) : 0.0)//(log(n_rs(k, l_i) + (1e6) * pi_s(k, l_i) + 1e-8) - log(n_r(k) + 1e6 + 1e-8)) : 0.0) 
         + (me_ ? log(m_rg(k, g_i) + N_rg(k, g_i) + alpha[k] + 1e-8) : log(lambda[k]+ 1e-8) + log(theta(k, g_i) + 1e-8))// + N_g(g_i)); //+ log(theta(k, geo[i]) + 1e-8) + N_rg(k, g_i) +
        ;
        if(which_names > 0){
          probs[k] += (kw_fname ? log(n_rf(k, first_name[i]) + psi * pi_f(k, first_name[i]) + 1e-8) - log(n_rf(k, first_name[i]) + psi + 1e-8): 0.0);//(log(n_rf(k, first_name[i]) + (1e6) * pi_f(k, first_name[i]) + 1e-8) - log(n_r(k) + 1e6+ 1e-8)) : 0.0) ;
          if(which_names > 1){
            probs[k] += (kw_mname ? log(n_rm(k, mid_name[i]) + psi * pi_m(k, mid_name[i]) + 1e-8) - log(n_rm(k, mid_name[i]) + psi + 1e-8) : 0.0);//(log(n_rm(k, mid_name[i]) + (1e6) * pi_m(k, mid_name[i]) + 1e-8) - log(n_r(k) + 1e6 + 1e-8)) : 0.0);
          }
        }
      }
      // exponentiate probabilities (using logsumexp)
      probs -= max(probs);
      probs = arma::exp(probs);
      probs /= arma::accu(probs);
      probs = arma::cumsum(probs);
      // //Sample race
      new_r = random_discrete(probs);
      race[i] = new_r;

      //Increment counts
       n_r(new_r)++;
       n_rs(new_r, l_i)++;
       m_rg(new_r, g_i)++;
       if(which_names > 0){
         n_rf(new_r, first_name[i])++;
         if(which_names > 1){
          n_rm(new_r, mid_name[i])++;
         }
       }
      // 

      //Store sample
      if(iter >= burnin){
        r_samp(i, new_r)++;
      }
    }
    
   // progress_bar.increment();
  }
  
  return(r_samp);
}