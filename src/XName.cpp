#include "XName.h"

using namespace Eigen;
using namespace Rcpp;
using namespace std;


XName::XName(const List& data,
	     const List& ctrl
	     ) :
  phi_tilde(as<MatrixXd>(data["census_table"])),
  gamma_prior(as<Vector2d>(ctrl["gamma_prior"])),
  beta_w(as<double>(ctrl["beta_prior"])),
  name_n(as<int>(data["n_unique_names"])),
  W(as< ListOf<IntegerVector> >(data["record_name_id"])),
  keywords(as< ListOf<IntegerVector> >(data["keynames"]))
{
  //initialize C (0 for non-keyword names, bern(0.7) otherwise)
  //Init n_rc matrix (?)
  //Init n_wr matrix (?)
}

void XName::sample_c(int r,
                    int voter,
                    int geo_id)
{
  //Get unique names...
  w = W[geo_id][voter];
  //(Check if name is in keyword list, and skip update if not)
  if(found_in(keywords[r], w)){	
        return;
  }

  // ... and current mixture component
  c = C[geo_id][voter];
  
  // remove data
  n_rw(w, r)--;
  n_rc(r, c)--;
  
  // newprob_c0
  c1_prob = phi_tilde(w, r) * (n_rc(r, 1) + gamma_prior[0]);
  
  // newprob_c0
  numerator = (n_rw(w, r) + beta_w)
    * (n_rc(r, 0) + gamma_prior[1]);
  denominator = n_rc(r, 0) + ((double)N_ * beta_w);
  c0_prob = numerator / denominator;
  
  
  // Normalize
  sum_c = c0_prob + c1_prob;
  
  c1_prob = c1_prob / sum_c;
  new_c = (R::runif(0,1) <= c1_prob);  //new_c = Bern(c1_prob);
  
  // add back data counts
  if (new_c == 0) {
    n_rw(w, r)++;
  }
  n_rc(r, new_c)++;
  
  //Update mixture component
  C[geo_id][voter] = new_c;
}

void XName::phihat_store(){
  // keyword component
  for(int r = 0; r < R_; ++r){
    denominator =  n_rc(r, 1) + n_rc(r, 0)
      + gamma_prior.sum();
    denominator_phi = n_r(r) + ((double)N_ * beta_w);
    pi_0 = (n_rc(r, 1) + gamma_prior[0]) / denominator;
    pi_1 = (n_rc(r, 0) + gamma_prior[1]) / denominator;
    for(int w = 0; w < N_; ++w){
      e_phi(w, r) += const_mean
	* (pi_0 * phi_tilde(w, r)
	   + pi_1 * ((n_rw(w, r) + beta_w)/denominator_phi));
    }
  }
}

MatrixXd XName::getPhiHat(){
  return e_phi;
}


