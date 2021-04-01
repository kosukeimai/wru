#ifndef __keyWRU__
#define __keyWRU__

#include <RcppEigen.h>
#include <vector>
#include <progress.hpp>
#include <progress_bar.hpp>
#include "XName.h"
#include "aux.h"


using namespace Eigen;
using namespace Rcpp;

class keyWRU
{
  // This is a header file for "WRU" class.
  // keyWRU class includes variables and functions
  // that appear throughout the keyWRU models.
  // Potential extensions inherit keyWRU and add model specific
  // functions.
  //
  // keyWRU uses virtual inheritance.
  // keyWRU is friends with the XName class,
  // which contains First-, Last-, etc. name
  // specific data and sampling methods

public:
 
  //Constructor/destructor    
  keyWRU(const List data,
	 const List ctrl
	 );

   //Methods
  void sample();
  List return_obj();  
  
private:
  // Methods
  int sample_r(int voter,
	       int geo_id);
  void iteration_single(int it);
  double loglik_total();
  void mfit_store();
  void phihat_store();

  

  //Data

  const int M_ // number of name types 
     , R_ // number of races
    , G_ // number of locations
    , max_iter // number of MCMC iterations
    , thin // thinning interval
    , llk_per // log_posterior store interval
    ;
  const bool verbose;
  const MatrixXd theta;
  const VectorXi geo_each_size; //
  
  int geo_id_ // location id placeholder
    , geo_size // Nr. of voters in location placeholder
    , w // name placeholder
    , r // race placeholder
    , c // mixture component placeholder
    , new_r // new race placeholder
    , voter_ // voter id placeholder
    , N_ // number of unique names placeholder
    ;

  //Intermediate computation placeholders 
  double numerator
    , denominator 
    , sum_r
    , n_rc // race-component ss. placeholder 
    ;
  
  std::vector<int> geo_indeces // vector of shuffled locations
    , record_indeces // vector of shuffl
    ;
  
  std::vector<VectorXi> R;
  std::vector<XName> names;
  
  
  VectorXd z_prob_vec,
    n_r;
  
  XName& name;
  
  
  
  
  
#endif
  
