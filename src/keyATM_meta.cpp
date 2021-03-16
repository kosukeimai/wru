#include "keyATM_meta.h"

using namespace Eigen;
using namespace Rcpp;
using namespace std;

# define PI_V   3.14159265358979323846  /* pi */

keyATMmeta::keyATMmeta(List model_, const int iter_)
{
  // Constructor
  model = model_;
  iter = iter_;
}


keyATMmeta::~keyATMmeta()
{
  // Destructor
}


void keyATMmeta::fit()
{
  // Read data, initialize the model and fit the model
  read_data();
  initialize();
  iteration();
}


void keyATMmeta::read_data()
{
  // `common` reads data required in all models
  // `specific` reads model specific data
  read_data_common();
  read_data_specific();
}


void keyATMmeta::read_data_common()
{
  // Read data
  W = model["W"]; Z = model["Z"]; S = model["S"];
  vocab = model["vocab"];
  regular_k = model["no_keyword_topics"];
  keywords_list = model["keywords"];
  keyword_k = model["keyword_k"];
  model_fit = model["model_fit"];

  num_topics = keyword_k + regular_k;

  // document-related constants
  num_vocab = vocab.size();
  num_doc = W.size();
  // alpha -> specific function  
  
  // Options
  options_list = model["options"];
  use_weights = options_list["use_weights"];
  slice_A = options_list["slice_shape"];
  store_theta = options_list["store_theta"];
  store_pi = options_list["store_pi"];
  thinning = options_list["thinning"];
  llk_per = options_list["llk_per"];
  verbose = options_list["verbose"];
  weights_type = Rcpp::as<std::string>(options_list["weights_type"]);

  // Priors
  priors_list = model["priors"];
  beta = priors_list["beta"];

  prior_gamma = MatrixXd::Zero(num_topics, 2);
  NumericMatrix RMatrix = priors_list["gamma"];
  prior_gamma = Rcpp::as<Eigen::MatrixXd>(RMatrix);
  beta_s = priors_list["beta_s"];

  // Stored values
  stored_values = model["stored_values"];

  // Slice Sampling
  // this is used except cov models
  model_settings = model["model_settings"];
  min_v = model_settings["slice_min"];
  min_v = shrinkp(min_v);

  max_v = model_settings["slice_max"];
  max_v = shrinkp(max_v);
}


void keyATMmeta::initialize()
{
  // `common`: common initialization
  // `specific`: model specific initialization
  initialize_common();
  initialize_specific();
}


void keyATMmeta::initialize_common()
{
  // Parameters
  eta_1 = 1.0;
  eta_2 = 1.0;
  eta_1_regular = 2.0;
  eta_2_regular = 1.0;

  // Slice sampling initialization for alpha
  // this is used except cov models
  max_shrink_time = 200;

  // Vector that stores keywords (words in dictionary)
  int wd_id;
  IntegerVector wd_ids;
  for (int ii = 0; ii < keyword_k; ++ii) {
    wd_ids = keywords_list[ii];
    keywords_num.push_back(wd_ids.size());
    
    std::unordered_set<int> keywords_set;
    for (int jj = 0; jj < wd_ids.size(); ++jj) {
      wd_id = wd_ids(jj);
      keywords_set.insert(wd_id);
    }
    
    keywords.push_back(keywords_set);
  }

  for (int i = keyword_k; i < num_topics; ++i) {
    std::unordered_set<int> keywords_set{ -1 };
  
    keywords_num.push_back(0);
    keywords.push_back(keywords_set);
  }

  // storage for sufficient statistics and their margins
  n_s0_kv = MatrixXd::Zero(num_topics, num_vocab);
  n_s1_kv.resize(num_topics, num_vocab);
  n_dk = MatrixXd::Zero(num_doc, num_topics);
  n_dk_noWeight = MatrixXd::Zero(num_doc, num_topics);
  n_s0_k = VectorXd::Zero(num_topics);
  n_s1_k = VectorXd::Zero(num_topics);
  vocab_weights = VectorXd::Constant(num_vocab, 1.0);

  int s, z, w;
  int doc_len;
  IntegerVector doc_s, doc_z, doc_w;


  //
  // Construct vocab weights
  //
  for (int doc_id = 0; doc_id < num_doc; ++doc_id) {
    doc_w = W[doc_id];
    doc_len = doc_w.size();
    doc_each_len.push_back(doc_len);
  
    for (int w_position = 0; w_position < doc_len; ++w_position) {
      w = doc_w[w_position];
      vocab_weights(w) += 1.0;
    }
  }
  total_words = (int)vocab_weights.sum();

  if (weights_type == "inv-freq" || weights_type == "inv-freq-normalized") {
    // Inverse frequency
    weights_invfreq(); 
  } else if (weights_type == "information-theory" || 
             weights_type == "information-theory-normalized") 
  {
    // Information theory 
    weights_inftheory();
  }
    
  // Normalize weights
  if (weights_type == "inv-freq-normalized" || 
      weights_type == "information-theory-normalized") {
    weights_normalize_total(); 
  } 

  // Do you want to use weights?
  if (use_weights == 0) {
    Rcpp::Rcout << "Not using weights!! Check `options$use_weights`." << std::endl;
    vocab_weights = VectorXd::Constant(num_vocab, 1.0);
  }

  
  //
  // Construct data matrices
  // 
  vector<Triplet> trip_s1;  // for a sparse matrix
  total_words_weighted = 0.0;
  double temp;

  for (int doc_id = 0; doc_id < num_doc; ++doc_id) {
    doc_s = S[doc_id], doc_z = Z[doc_id], doc_w = W[doc_id];
    doc_len = doc_each_len[doc_id];

    for (int w_position = 0; w_position < doc_len; ++w_position) {
      s = doc_s[w_position], z = doc_z[w_position], w = doc_w[w_position];
      if (s == 0){
        n_s0_kv(z, w) += vocab_weights(w);
        n_s0_k(z) += vocab_weights(w);
      } else {
        trip_s1.push_back(Triplet(z, w, vocab_weights(w)));
        n_s1_k(z) += vocab_weights(w);
      }
      n_dk(doc_id, z) += vocab_weights(w);
      n_dk_noWeight(doc_id, z) += 1.0;
    }

    temp = n_dk.row(doc_id).sum();
    doc_each_len_weighted.push_back(temp);
    total_words_weighted += temp;
  }
  n_s1_kv.setFromTriplets(trip_s1.begin(), trip_s1.end());
  

  // Use during the iteration
  z_prob_vec = VectorXd::Zero(num_topics);

  // Use labels to initialize beta (prior for topic-word distributions)
  // if (model_settings.containsElementNamed("labels")) {
  //   use_labels = 1;
  //   initialize_betas();  // do not use labels for beta for now
  // } else {
  //   use_labels = 0;
  //   Vbeta = (double)num_vocab * beta;
  //
  //   Lbeta_sk = VectorXd::Zero(num_topics);
  //   for (int k = 0; k < num_topics; k++) {
  //     Lbeta_sk(k) = (double)keywords_num[k] * beta_s; 
  //   }
  // }
  use_labels = 0;
  Vbeta = (double)num_vocab * beta;

  Lbeta_sk = VectorXd::Zero(num_topics);
  for (int k = 0; k < num_topics; ++k) {
    Lbeta_sk(k) = (double)keywords_num[k] * beta_s;
  }
  
}


void keyATMmeta::weights_invfreq()
{
  // Inverse frequency
  vocab_weights = (double)total_words / vocab_weights.array();
}


void keyATMmeta::weights_inftheory()
{
  // Information Theory
  vocab_weights = vocab_weights.array() / (double)total_words;
  vocab_weights = vocab_weights.array().log();
  vocab_weights = - vocab_weights.array() / log(2);  
}


void keyATMmeta::weights_normalize_total()
{
  // Normalize weights so that the total weighted count
  // is the same as the total length of documents

  double total_weights = 0.0;
  int doc_len;
  int w;
  for (int doc_id = 0; doc_id < num_doc; ++doc_id) {
    doc_w = W[doc_id];
    doc_len = doc_each_len[doc_id];

    for (int w_position = 0; w_position < doc_len; ++w_position) {
      w = doc_w[w_position];
      total_weights += vocab_weights(w);
    }
  }
  vocab_weights = vocab_weights.array() * (double)total_words / total_weights;
}


void keyATMmeta::initialize_betas()
{
  // Initialize betas using the label information
  IntegerVector label_vec = model_settings["labels"];
  int label;
  int v;
  int doc_len;
  IntegerVector doc_w;

  // Initialize beta matrix
  beta_s0kv = MatrixXd::Constant(num_topics, num_vocab, beta);
  beta_s1kv.resize(num_topics, num_vocab);

  vector<Triplet> trip_beta_s1;

  for (int k = 0; k < keyword_k; ++k) {
    for (auto &v : keywords[k]) {
      trip_beta_s1.push_back(Triplet(k, v, beta_s));
    } 
  }


  // Add values based on the observed counts
  for (int doc_id = 0; doc_id < num_doc; ++doc_id) {
    label = label_vec[doc_id];
    if (label < 0)
      continue;
  
    doc_w = W[doc_id]; 
    doc_len = doc_each_len[doc_id];
  
    for (int w_pos = 0; w_pos < doc_len; ++w_pos) {
      v = doc_w[w_pos];
  
      if (use_weights) {
        beta_s0kv(label, v) += vocab_weights(v);
  
        if (keywords[label].find(v) != keywords[label].end()){
          trip_beta_s1.push_back(Triplet(label, v, vocab_weights(v)));
        }
      } else {
        beta_s0kv(label, v) += 1.0;
  
        if (keywords[label].find(v) != keywords[label].end()){
          trip_beta_s1.push_back(Triplet(label, v, vocab_weights(v)));
        }
      }
    }
  }

  // Make beta_s1kv as a sparse matrix
  beta_s1kv.setFromTriplets(trip_beta_s1.begin(), trip_beta_s1.end());

  // Pre-Cauculation for speed up
  Vbeta_k = beta_s0kv.rowwise().sum();
  Lbeta_sk = beta_s1kv * VectorXd::Ones(beta_s1kv.cols()); // beta_s1kv.rowwise().sum();
}


void keyATMmeta::iteration()
{
  // Iteration
  Progress progress_bar(iter, !(bool)verbose);

  for (int it = 0; it < iter; ++it) {
    // Run iteration
    iteration_single(it); 

    // Check storing values
    int r_index = it + 1;
    if (r_index % llk_per == 0 || r_index == 1 || r_index == iter) {
      sampling_store(r_index);
      verbose_special(r_index);
    }
    if (r_index % thinning == 0 || r_index == 1 || r_index == iter) {
      parameters_store(r_index);
    }

    // Progress bar
    progress_bar.increment();

    // Check keybord interruption to cancel the iteration
    checkUserInterrupt();
  }

  model["model_fit"] = model_fit;
}


void keyATMmeta::sampling_store(int r_index)
{
  // Store likelihood and perplexity during the sampling
 
  double loglik;
  loglik = (use_labels) ? loglik_total_label() : loglik_total();
  double perplexity = exp(-loglik / (double)total_words_weighted);

  NumericVector model_fit_vec;
  model_fit_vec.push_back(r_index);
  model_fit_vec.push_back(loglik);
  model_fit_vec.push_back(perplexity);
  model_fit.push_back(model_fit_vec);
  
  if (verbose) {
    Rcerr << "[" << r_index << "] log likelihood: " << loglik <<
             " (perplexity: " << perplexity << ")" << std::endl;
  }
}


void keyATMmeta::parameters_store(int r_index)
{
  if (store_theta)
    store_theta_iter(r_index);

  if (store_pi)
    store_pi_iter(r_index);
}


// void keyATMmeta::store_theta_iter(int r_index)
// {
//   Z_tables = stored_values["Z_tables"];
//   NumericMatrix Z_table = Rcpp::wrap(n_dk_noWeight);
//   Z_tables.push_back(Z_table);
//   stored_values["Z_tables"] = Z_tables;
// }


void keyATMmeta::store_pi_iter(int r_index)
{
  List pi_vectors = stored_values["pi_vectors"];
  // calculate
  VectorXd numer = n_s1_k.array() + prior_gamma.col(0).array();
  VectorXd denom = n_s0_k.array() + prior_gamma.col(1).array() + numer.array();
  VectorXd pi = numer.array() / denom.array();

  // store
  NumericVector pi_vector = Rcpp::wrap(pi);
  pi_vectors.push_back(pi_vector);
  stored_values["pi_vectors"] = pi_vectors;
}


void keyATMmeta::verbose_special(int r_index)
{
  // If there is anything special to show, write here.
}


//
// Sampling
//
int keyATMmeta::sample_z(VectorXd &alpha, int z, int s,
                         int w, int doc_id)
{
  int new_z;
  double numerator, denominator;
  double sum;

  // remove data
  if (s == 0) {
    n_s0_kv(z, w) -= vocab_weights(w);
    n_s0_k(z) -= vocab_weights(w);
  } else if (s == 1) {
    n_s1_kv.coeffRef(z, w) -= vocab_weights(w);
    n_s1_k(z) -= vocab_weights(w);
  } else {
    Rcerr << "Error at sample_z, remove" << std::endl;
  }

  n_dk(doc_id, z) -= vocab_weights(w);
  n_dk_noWeight(doc_id, z) -= 1.0;

  new_z = -1; // debug
  if (s == 0) {
    for (int k = 0; k < num_topics; ++k) {

      numerator = (beta + n_s0_kv(k, w)) *
        (n_s0_k(k) + prior_gamma(k, 1)) *
        (n_dk(doc_id, k) + alpha(k));

      denominator = (Vbeta + n_s0_k(k)) *
        (n_s1_k(k) + prior_gamma(k, 0) + n_s0_k(k) + prior_gamma(k, 1));

      z_prob_vec(k) = numerator / denominator;
    }

    sum = z_prob_vec.sum(); // normalize
    new_z = sampler::rcat_without_normalize(z_prob_vec, sum, num_topics); // take a sample

  } else {
    for (int k = 0; k < num_topics; ++k) {
      if (keywords[k].find(w) == keywords[k].end()) {
        z_prob_vec(k) = 0.0;
        continue;
      } else { 
        numerator = (beta_s + n_s1_kv.coeffRef(k, w)) *
          (n_s1_k(k) + prior_gamma(k, 0)) *
          (n_dk(doc_id, k) + alpha(k));
        denominator = (Lbeta_sk(k) + n_s1_k(k) ) *
          (n_s1_k(k) + prior_gamma(k, 0) + n_s0_k(k) + prior_gamma(k, 1));

        z_prob_vec(k) = numerator / denominator;
      }
    }

    sum = z_prob_vec.sum();
    new_z = sampler::rcat_without_normalize(z_prob_vec, sum, num_topics); // take a sample

  }

  // add back data counts
  if (s == 0) {
    n_s0_kv(new_z, w) += vocab_weights(w);
    n_s0_k(new_z) += vocab_weights(w);
  } else if (s == 1) {
    n_s1_kv.coeffRef(new_z, w) += vocab_weights(w);
    n_s1_k(new_z) += vocab_weights(w);
  } else {
    Rcerr << "Error at sample_z, add" << std::endl;
  }
  n_dk(doc_id, new_z) += vocab_weights(w);
  n_dk_noWeight(doc_id, new_z) += 1.0;

  return new_z;
}


// int keyATMmeta::sample_z_label(VectorXd &alpha, int z, int s,
//                          int w, int doc_id)
// {
//   int new_z;
//   double numerator, denominator;
//   double sum;

//   // For labeled data!
//   // remove data
//   if (s == 0) {
//     n_s0_kv(z, w) -= vocab_weights(w);
//     n_s0_k(z) -= vocab_weights(w);
//   } else if (s == 1) {
//     n_s1_kv.coeffRef(z, w) -= vocab_weights(w);
//     n_s1_k(z) -= vocab_weights(w);
//   } else {
//     Rcerr << "Error at sample_z, remove" << std::endl;
//   }

//   n_dk(doc_id, z) -= vocab_weights(w);
//   n_dk_noWeight(doc_id, z) -= 1.0;

//   new_z = -1; // debug
//   if (s == 0) {
//     for (int k = 0; k < num_topics; ++k) {

//       numerator = (beta_s0kv(k, w) + n_s0_kv(k, w)) *
//         (n_s0_k(k) + prior_gamma(k, 1)) *
//         (n_dk(doc_id, k) + alpha(k));

//       denominator = (Vbeta_k(k) + n_s0_k(k)) *
//         (n_s1_k(k) + prior_gamma(k, 0) + n_s0_k(k) + prior_gamma(k, 1));

//       z_prob_vec(k) = numerator / denominator;
//     }

//     sum = z_prob_vec.sum(); // normalize
//     new_z = sampler::rcat_without_normalize(z_prob_vec, sum, num_topics); // take a sample

//   } else {
//     for (int k = 0; k < num_topics; ++k) {
//       if (keywords[k].find(w) == keywords[k].end()) {
//         z_prob_vec(k) = 0.0;
//         continue;
//       } else { 
//         numerator = (beta_s1kv.coeffRef(k, w) + n_s1_kv.coeffRef(k, w)) *
//           (n_s1_k(k) + prior_gamma(k, 0)) *
//           (n_dk(doc_id, k) + alpha(k));
//         denominator = (Lbeta_sk(k) + n_s1_k(k) ) *
//           (n_s1_k(k) + prior_gamma(k, 0) + n_s0_k(k) + prior_gamma(k, 1));

//         z_prob_vec(k) = numerator / denominator;
//       }
//     }


//     sum = z_prob_vec.sum();
//     new_z = sampler::rcat_without_normalize(z_prob_vec, sum, num_topics); // take a sample

//   }

//   // add back data counts
//   if (s == 0) {
//     n_s0_kv(new_z, w) += vocab_weights(w);
//     n_s0_k(new_z) += vocab_weights(w);
//   } else if (s == 1) {
//     n_s1_kv.coeffRef(new_z, w) += vocab_weights(w);
//     n_s1_k(new_z) += vocab_weights(w);
//   } else {
//     Rcerr << "Error at sample_z, add" << std::endl;
//   }
//   n_dk(doc_id, new_z) += vocab_weights(w);
//   n_dk_noWeight(doc_id, new_z) += 1.0;

//   return new_z;
// }



int keyATMmeta::sample_s(int z, int s, int w, int doc_id)
{
  int new_s;
  double numerator, denominator;
  double s0_prob;
  double s1_prob;
  double sum;

  // remove data
  if (s == 0) {
    n_s0_kv(z, w) -= vocab_weights(w);
    n_s0_k(z) -= vocab_weights(w);
  } else {
    n_s1_kv.coeffRef(z, w) -= vocab_weights(w);
    n_s1_k(z) -= vocab_weights(w);
  }

  // newprob_s1()

  numerator = (beta_s + n_s1_kv.coeffRef(z, w)) *
    ( n_s1_k(z) + prior_gamma(z, 0) );
  denominator = (Lbeta_sk(z) + n_s1_k(z) );
  s1_prob = numerator / denominator;

  // newprob_s0()
  numerator = (beta + n_s0_kv(z, w)) *
    (n_s0_k(z) + prior_gamma(z, 1));

  denominator = (Vbeta + n_s0_k(z) );
  s0_prob = numerator / denominator;

  // Normalize
  sum = s0_prob + s1_prob;

  s1_prob = s1_prob / sum;
  new_s = R::runif(0,1) <= s1_prob;  //new_s = Bern(s0_prob, s1_prob);

  // add back data counts
  if (new_s == 0) {
    n_s0_kv(z, w) += vocab_weights(w);
    n_s0_k(z) += vocab_weights(w);
  } else {
    n_s1_kv.coeffRef(z, w) += vocab_weights(w);
    n_s1_k(z) += vocab_weights(w);
  }

  return new_s;
}


// int keyATMmeta::sample_s_label(VectorXd &alpha, int z, int s,
//                  int w, int doc_id)
// {
//   int new_s;
//   double numerator, denominator;
//   double s0_prob;
//   double s1_prob;
//   double sum;

//   // For labeled data!
//   // remove data
//   if (s == 0) {
//     n_s0_kv(z, w) -= vocab_weights(w);
//     n_s0_k(z) -= vocab_weights(w);
//   } else {
//     n_s1_kv.coeffRef(z, w) -= vocab_weights(w);
//     n_s1_k(z) -= vocab_weights(w);
//   }

//   // newprob_s1()

//   numerator = (beta_s1kv.coeffRef(z, w) + n_s1_kv.coeffRef(z, w)) *
//     ( n_s1_k(z) + prior_gamma(z, 0) );
//   denominator = (Lbeta_sk(z) + n_s1_k(z) );
//   s1_prob = numerator / denominator;

//   // newprob_s0()
//   numerator = (beta_s0kv(z, w) + n_s0_kv(z, w)) *
//     (n_s0_k(z) + prior_gamma(z, 1));

//   denominator = (Vbeta_k(z) + n_s0_k(z) );
//   s0_prob = numerator / denominator;

//   // Normalize
//   sum = s0_prob + s1_prob;

//   s1_prob = s1_prob / sum;
//   new_s = R::runif(0,1) <= s1_prob;  //new_s = Bern(s0_prob, s1_prob);

//   // add back data counts
//   if (new_s == 0) {
//     n_s0_kv(z, w) += vocab_weights(w);
//     n_s0_k(z) += vocab_weights(w);
//   } else {
//     n_s1_kv.coeffRef(z, w) += vocab_weights(w);
//     n_s1_k(z) += vocab_weights(w);
//   }

//   return new_s;
// }


// double keyATMmeta::loglik_total_label()
// {
//   // Should be defined in each model
//   return 0.0;
// }


// Utilities
//
double keyATMmeta::gammapdfln(const double x, const double a, const double b)
{
  // a: shape, b: scale
  return - a * log(b) - mylgamma(a) + (a-1.0) * log(x) - x/b;
}


double keyATMmeta::betapdf(const double x, const double a, const double b)
{
  return tgamma(a+b) / (tgamma(a) * tgamma(b)) * pow(x, a-1) * pow(1-x, b-1);
}


double keyATMmeta::betapdfln(const double x, const double a, const double b)
{
  return (a-1)*log(x) + (b-1)*log(1.0-x) + mylgamma(a+b) - mylgamma(a) - mylgamma(b);
}


NumericVector keyATMmeta::alpha_reformat(VectorXd& alpha, int num_topics)
{
  NumericVector alpha_rvec(num_topics);

  for (int i = 0; i < num_topics; ++i) {
    alpha_rvec[i] = alpha(i);
  }

  return alpha_rvec;
}


double keyATMmeta::gammaln_frac(const double value, const int count)
{
  // Calculate \log \frac{\gamma(value + count)}{\gamma(\value)}
  // Not very fast
  double gammaln_val;
  
  if (count > 19) {
    return mylgamma(value + count) - mylgamma(value);  
  } else {
    gammaln_val = 0.0;

    for (int i = 0; i < count; ++i) {
      gammaln_val += log(value + i);  
    }

    return gammaln_val;
  }
}


List keyATMmeta::return_model()
{
  // Return output to R

  if (use_labels) {
    // Return prior to use in R 
    NumericMatrix R_betas0 = Rcpp::wrap(beta_s0kv);
    SEXP R_betas1 = Rcpp::wrap(beta_s1kv);
    
    priors_list.push_back(R_betas0, "beta_s0");
    priors_list.push_back(R_betas1, "beta_s1");
    model["priors"] = priors_list;
  }

  // Vocabulary weights
  NumericVector vocab_weights_R = stored_values["vocab_weights"];

  for (int v = 0; v < num_vocab; v++) {
    vocab_weights_R[v] = vocab_weights(v); 
  }
  stored_values["vocab_weights"] = vocab_weights_R;
  model["stored_values"] = stored_values;

  model["stored_values"] = stored_values;

  return model;
}
