#ifndef __XName__
#define __XName__

#include <RcppEigen.h>
#include <vector>

using namespace Eigen;
using namespace Rcpp;

class XName
{
public:
  //Constructor
  XName(const List& data,
	const List& ctrl
	);
  
  //getters
  MatrixXd getPhiHat();

private:
  //Data
    const ListOf<IntegerVector> W, // List of vectors of unique name id for each voter, for each geo
    keywords; //List of vectors of keyword name id.
  const MatrixXd phi_tilde; // census table, name by race
  const Vector2d gamma_prior;
  const double beta_w;
  const int name_n;
  
  
  std::vector<VectorXi> C; // Vector of vectors of mixture component for each record, in each geo
  MatrixXd n_rc, // Suff. stat, race by mixture component
   n_wr; //Suff stat, name by race for names in non-keyword components

  double c1_prob,
    numerator,
    denominator,
    c0_prob,
    sum_c
    ;
  int c,
    w,
    new_c
  ;
      //Methods
  void sample_c(int r,
		int voter,
		int geo_id)

  void phihat_store();
  
  
  
  friend class keyWRU;
};
#endif
