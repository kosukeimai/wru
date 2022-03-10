#include "sample_me.h"

//' Collapsed Gibbs sampler for hWRU. Internal function
//'
//' @param last_name Integer vector of last name identifiers for each record (zero indexed; as all that follow). Must match columns numbers in M_rs.  
//' @param first_name See last_name
//' @param middle_name See last_name
//' @param geo Integer vector of geographic units for each record. Must match column number in N_rg
//' @param N_rg Integer matrix of race | geography counts in census (geograpgies in columns).
//' @param M_rs Integer matrix of race | surname counts in dictionary (surnames in columns).
//' @param M_rf Same as `M_rs`, but for first names (can be empty matrix for surname only models).
//' @param M_rm Same as `M_rs`, but for middle names (can be empty matrix for surname, or surname and first name only models).
//' @param alpha Numeric matrix of race | geography prior probabilities.
//' @param pi_s Numeric matrix of race | surname prior probabilities.
//' @param pi_f Same as `pi_s`, but for first names.
//' @param pi_m Same as `pi_s`, but for middle names.
//' @param pi_nr Vector of marginal probability distribution over race categories; non-keyword names default to this distribution.
//' @param which_names Integer; 0=surname only. 1=surname + first name. 2= surname, first, and middle names.
//' @param samples Integer number of samples to take after (in total)
//' @param burnin Integer number of samples to discard as burn-in of Markov chain
//' @param me_race Boolean; should measurement error in race | geography be corrected?
//' @param me_name Boolean; should measurement error in race | names be corrected?
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
                     const arma::umat& M_rs,
                     const arma::umat& M_rf,
                     const arma::umat& M_rm,
                     const arma::mat& alpha,
                     const arma::mat& pi_s,
                     const arma::mat& pi_f,
                     const arma::mat& pi_m,
                     const arma::vec& pi_nr,
                     const arma::uword which_names,
                     const arma::uword samples,
                     const arma::uword burnin,
                     const bool me_name,
                     const bool me_race,
                     const arma::uvec& race_init,
                     const bool verbose)
{ 
  arma::uword max_iter = samples + burnin;
  arma::uword N = last_name.n_elem; // Nr. of vf records 
  arma::uword J = pi_s.n_rows; // Nr. of racial cats
  arma::uword K = arma::max(last_name) + 1; //Nr. of surnames
  arma::uword L = arma::max(geo) + 1; //Nr. of locations
  arma::uword S_KW = pi_s.n_cols;
  arma::uword F_KW, M_KW;
  if(which_names > 0){
    F_KW = pi_f.n_cols;
    if(which_names > 1){
      M_KW = pi_m.n_cols;
    }
  }
  arma::umat r_samp(N, J, arma::fill::zeros);
  arma::uvec race(&race_init[0], N);
  
  //Temporary storage
  arma::uvec m_r(J, arma::fill::zeros);
  arma::umat m_rs(J, K, arma::fill::zeros);
  arma::umat n_rg(J, L, arma::fill::zeros);
  arma::umat m_rf, m_rm;
  if(which_names > 0){
    m_rf = m_rf.zeros(J, arma::max(first_name) + 1);
    if(which_names > 1){
      m_rm = m_rm.zeros(J, arma::max(mid_name) + 1);
    }
  }
  
  //Initialize global counts
  arma::uword r_i = 0, l_i = 0, g_i = 0;
  for(arma::uword i = 0; i < N; ++i) {
    r_i = race_init[i];
    m_r(r_i)++;
    m_rs(r_i, last_name[i])++;
    n_rg(r_i, geo[i])++;
    if(which_names > 0){
      m_rf(r_i, first_name[i])++;
      if(which_names > 1){
        m_rm(r_i, mid_name[i])++;
      }
    }
  }
  const arma::uvec Ms_r = arma::sum(M_rs, 1);
  arma::uvec Mf_r, Mm_r;
  if(which_names > 0){
    Mf_r = arma::sum(M_rf, 1);
    if(which_names > 1){
      Mm_r = arma::sum(M_rm, 1);
    }
  }
  
  arma::vec probs(J, arma::fill::zeros);
  arma::uword new_r = 0;
  bool kw_lname = true, kw_fname = true, kw_mname = true; 
  for (arma::uword iter = 0; iter < max_iter; ++iter) {
    // Check stop signal
    Rcpp::checkUserInterrupt();
    // Sample from full conditionals
    for(arma::uword i = 0; i < N; ++i) {
      
      r_i = race[i]; l_i = last_name[i]; g_i = geo[i];
      m_r(r_i)--;
      n_rg(r_i, g_i)--;
      m_rs(r_i, l_i)--;
      kw_lname = l_i < S_KW;
      if(which_names > 0){
        kw_fname = first_name[i] < F_KW;
        m_rf(r_i, first_name[i])--;
        if(which_names > 1){
          kw_mname = mid_name[i] < M_KW;
          m_rm(r_i, mid_name[i])--;
        }
      }
      probs.zeros();
      for(arma::uword j = 0; j < J; ++j){
        probs[j] = 0.0;
        // surname
        if(kw_lname){
          probs[j] += me_name ? log(m_rs(j, l_i) + M_rs(j, l_i) + 1.0) - log(1.0 + Ms_r[j] + m_r[j]) : log(pi_s(j, l_i) + 1e-8);
        } else {
          probs[j] += log(pi_nr[j] + 1e-8);
        }
        // if(!probs.is_finite()){
        //   Rcpp::Rcout << "Surname probs: "<< std::endl << probs << std::endl;
        //   Rcpp::stop("wtf!");
        // }
        // geography
        probs[j] += me_race ? log(n_rg(j, g_i) + N_rg(j, g_i) + 1.0) : log(alpha(j, g_i) + 1e-8);
        
        //other names
        if(which_names > 0){
          if(kw_fname){
            probs[j] += me_name ? log(m_rf(j, first_name[i]) + M_rf(j, first_name[i]) + 1.0) - log(1.0 + Mf_r[j] + m_r[j]) : log(pi_f(j, first_name[i]));
          } else {
            probs[j] += log(pi_nr[j] + 1e-8);
          }
          if(which_names > 1){
            if(kw_mname){
              probs[j] += me_name ? log(m_rm(j, mid_name[i]) + M_rm(j, mid_name[i]) + 1.0) - log(1.0 + Mm_r[j] + m_r[j]) : log(pi_m(j, mid_name[i]));
            } else {
              probs[j] += log(pi_nr[j] + 1e-8);
            }
          }
        }
      }
      // exponentiate probabilities (using logsumexp)
      //Rcpp::Rcout << "Done" << std::endl;
      probs -= probs.max();
      probs = arma::exp(probs);
      probs /= arma::accu(probs);
      probs = arma::cumsum(probs);
      // //Sample race
      new_r = random_discrete(probs);
      // Rcpp::Rcout << "probs:" << probs << std::endl;
      // Rcpp::Rcout << "new_r" << new_r << std::endl;
      
      race[i] = new_r;
      
      //Increment counts
      m_r(new_r)++;
      
      m_rs(new_r, l_i)++;
      n_rg(new_r, g_i)++;
      if(which_names > 0){
        m_rf(new_r, first_name[i])++;
        if(which_names > 1){
          m_rm(new_r, mid_name[i])++;
        }
      }
      // 
      //Rcpp::Rcout << "Done for real" << std::endl;
      //Store sample
      if(iter >= burnin){
        r_samp(i, new_r)++;
      }
    }
  }
  
  return(r_samp);
}