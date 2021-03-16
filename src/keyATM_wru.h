#ifndef __keyATM_base__INCLUDED__
#define __keyATM_base__INCLUDED__

#include <Rcpp.h>
#include <RcppEigen.h>
#include <unordered_set>
#include "sampler.h"
#include "keyATM_meta.h"

using namespace Eigen;
using namespace Rcpp;
using namespace std;

class keyATMbase : virtual public keyATMmeta
{
  public:  
    //
    // Parameters
    //
    int estimate_alpha;
    int store_alpha;

    std::vector<int> topic_ids;
    VectorXd keep_current_param;
    double store_loglik;
    double newalphallk;

      // in alpha_loglik
      MatrixXd ndk_a;
    
    //
    // Functions
    //

    // Constructor
    keyATMbase(List model_, const int iter_) :
      keyATMmeta(model_, iter_) {};

    // Read data
    void read_data_specific();

    // Initialization
    void initialize_specific();

    // Iteration
    virtual void iteration_single(int it);
    void sample_parameters(int it);
    void sample_alpha();
    double alpha_loglik(int k);
    virtual double loglik_total();
    double loglik_total_label();
};


#endif

