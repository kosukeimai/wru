#ifndef __keyATMmeta__INCLUDED__
#define __keyATMmeta__INCLUDED__

#include <Rcpp.h>
#include <RcppEigen.h>
#include <unordered_set>
#include "sampler.h"

// RcppProgress
//   shows a progress bar during the iteration
#include <progress.hpp>
#include <progress_bar.hpp>

using namespace Eigen;
using namespace Rcpp;
using namespace std;

class keyATMmeta
{
  // This is a header file for "meta" class.
  // keyATMmeta class includes variables and functions
  // that appear throughout the keyATM models.
  // Each model inherits keyATMmeta and adds model specific
  // functions.
  //
  // keyATM uses virtual inheritance.
  //    All keyATM models inherit this class and add model
  //    specific functions.
  //
  //    weightedLDA models inherit this class but modify some
  //    initialization functions.

  public:
    //
    // Parameters
    //
    int iter;
    int llk_per;
    int verbose;
    string weights_type;

    double eta_1;
    double eta_2;
    double eta_1_regular;
    double eta_2_regular;
    int use_weights;
    int store_theta;
    int store_pi;
    int thinning;

    double slice_A; // parameter for slice sampling 

    //
    // Data
    //
    List model;
    List W, Z, S;
    StringVector vocab;
    NumericVector nv_alpha;
    MatrixXd prior_gamma;
    double beta, beta_s;
    double Vbeta;
    int regular_k, keyword_k;
    List keywords_list;
    List model_fit;
    std::vector<int> doc_each_len;
    std::vector<double> doc_each_len_weighted;
    
    int num_vocab, num_doc, total_words;
    double total_words_weighted;

    VectorXi labels_true;
    int use_labels;
    MatrixXd beta_s0kv;
    SparseMatrix<double, RowMajor> beta_s1kv;
    VectorXd Vbeta_k;
    VectorXd Lbeta_sk;

    List options_list;
    List Z_tables;
    List priors_list;
    List model_settings;
    List stored_values;
    NumericMatrix Z_table;

    // alpha
    int num_topics;
    VectorXd alpha;

    //
    // Keywords
    //
    std::vector< std::unordered_set<int> > keywords;
    std::vector<int> keywords_num;

    //
    // Latent Variables
    //
    MatrixXd n_s0_kv;
    SparseMatrix<double,RowMajor> n_s1_kv;
    typedef Eigen::Triplet<double> Triplet;
    MatrixXd n_dk;
    MatrixXd n_dk_noWeight;
    VectorXd n_s0_k;
    VectorXd n_s1_k;
    VectorXd vocab_weights;

    //
    // Use during the iteration
    //
      // Declaration
      std::vector<int> doc_indexes;
      std::vector<int> token_indexes;
      IntegerVector doc_s, doc_z, doc_w;
  
      // sample_z
      VectorXd z_prob_vec;

      // sample alpha
      double min_v;
      double max_v;
      int max_shrink_time;



    //
    // Functions
    //
    keyATMmeta(List model_, const int iter_);
    ~keyATMmeta();
    void fit();

    // Reading and Initialization
    void read_data();
      virtual void read_data_common();
      virtual void read_data_specific() = 0;

    void initialize();
      virtual void initialize_common();
      virtual void initialize_specific() = 0;

      void weights_invfreq();
      void weights_inftheory();
      void weights_normalize_total();

      void initialize_betas();

    //
    // Sampling
    //
    void iteration();
    virtual void iteration_single(int it) = 0;
    virtual void sample_parameters(int it) = 0;

    virtual int sample_z(VectorXd &alpha, int z, int s,
                         int w, int doc_id);
  //int sample_z_label(VectorXd &alpha, int z, int s,
  //                   int w, int doc_id);

    virtual int sample_s(int z, int s, int w, int doc_id);
  //int sample_s_label(VectorXd &alpha, int z, int s,
  //                   int w, int doc_id);

    void sampling_store(int r_index);
    virtual void parameters_store(int r_index);
  //void store_theta_iter(int r_index);
    void store_pi_iter(int r_index);

    virtual void verbose_special(int r_index);

    virtual double loglik_total() = 0;
    //virtual double loglik_total_label();

    //
    // Utilities
    //
    double vmax, vmin;

    double gammapdfln(const double x, const double a, const double b);
    double betapdf(const double x, const double a, const double b);
    double betapdfln(const double x, const double a, const double b);
    NumericVector alpha_reformat(VectorXd& alpha, int num_topics);

    double gammaln_frac(const double value, const int count);

    //
    // Inline functions
    //

    // Slice sampling
    double expand(const double p, const double A)
    {
      return (-(1.0/A) * log((1.0/p) - 1.0));
    };

    double shrink(const double x, const double A)
    {
      return (1.0 / (1.0 + exp(-A*x)));
    };

    double shrinkp(const double x)
    {
      return (x / (1.0 + x)); 
    };


    // Log-sum-exp
    double logsumexp(double x, double y, bool flg)
    {
      if (flg) return y; // init mode
      if (x == y) return x + 0.69314718055; // log(2)
      double vmin = std::min (x, y);
      double vmax = std::max (x, y);
      if (vmax > vmin + 50) {
        return vmax;
      } else {
        return vmax + std::log (std::exp (vmin - vmax) + 1.0);
      }
    };

    double logsumexp_Eigen(VectorXd &vec, const int size){
      double vmax = vec.maxCoeff();
      double sum = 0.0;

      for(int i = 0; i < size; i++){
        sum += exp(vec(i) - vmax);
      }
      
      return vmax + log(sum);
    }

    // Approximations
    double mylgamma(const double x){
      // gammaln_val = 0.0;
      // gammaln_val = lgamma(x);
      
      // Good approximation when x > 1
      //    x > 1: max abs err: 2.272e-03
      //    x > 0.5: 0.012
      //    x > 0.6: 0.008
      // Abramowitz and Stegun p.257
      
      if(x < 0.6)
        return (lgamma(x));
      else
        return ((x-0.5)*log(x) - x + 0.91893853320467 + 1/(12*x));
    };

    // Approximations below are not used
    double mypow(const double a, const double b){
      // Reference: https://github.com/ekmett/approximate/blob/master/cbits/fast.c
      // Probably not good to use if b>1.0
      
      if(b > 1.0)
        return(pow(a,b));

      union { double d; long long x; } u = { a };
      u.x = (long long)(b * (u.x - 4606921278410026770LL) + 4606921278410026770LL);
      return u.d;
    };

    double myexp(const double a){
      // Seems to be not very good
      union { double d; long long x; } u, v;
      u.x = (long long)(3248660424278399LL * a + 0x3fdf127e83d16f12LL);
      v.x = (long long)(0x3fdf127e83d16f12LL - 3248660424278399LL * a);
      return u.d / v.d;
    };

    double mylog(const double a){
      // Looks fine even with large a
      union { double d; long long x; } u = { a };
      return (u.x - 4606921278410026770) * 1.539095918623324e-16;  
    };

    List return_model();
  
};

#endif
