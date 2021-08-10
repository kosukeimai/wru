#ifndef __XName__
#define __XName__

#include <RcppEigen.h>
#include <vector>
#include "aux_funs.h"

using namespace Eigen;
using namespace Rcpp;

class XName
{
public:
  //Constructor
  XName(const List& data,
        const List& ctrl,
        const int G_,
        const int R_,
        const VectorXd& n_r,
        const std::vector<IntegerVector>& Races,
        const String type
  );
  
  //getters
  MatrixXd getPhiHat();
  
private:
  //Data
  const std::vector<IntegerVector> W; // List of vectors of unique name id (row 0) and counts (row 1), for each geo
  const std::vector<IntegerVector> keywords; //List of vectors of keyword name id (rows) by race (columns).
  const MatrixXd phi_tilde; // census table, name by race
  const VectorXd gamma_prior;
  const double beta_w;
  const int N_,
  R_
  ;
  const VectorXd& n_r;
  const String type;
  
  int max_kw;
  
  std::vector<VectorXi> C; // Vector of vectors of mixture component for each record, in each geo
  MatrixXd n_rc, // Suff. stat, race by mixture component
  e_phi, //Expected distribution over races for each name
  n_wr; //Suff stat, name by race for names in non-keyword components
  
  
  double c1_prob,
  numerator,
  denominator,
  denominator_phi,
  c0_prob,
  sum_c, 
  pi_0, pi_1;
  
  int c,
  new_c,
    r_,
    w_;
  //Methods
  void sample_c(int r,
                int voter,
                int geo_id);
  void phihat_store();

  
  
  friend class keyWRU;
};
#endif
