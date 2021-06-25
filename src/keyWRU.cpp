#include "keyWRU.h"

using namespace Eigen;
using namespace Rcpp;

keyWRU::keyWRU(const List data,
               const List ctrl) :
  M_(as<int>(data["name_type_n"])),
  R_(as<int>(data["race_n"])),
  G_(as<int>(data["geo_n"])),
  max_iter(as<int>(ctrl["iter"])),
  thin(as<int>(ctrl["thin"])),
  burnin(as<int>(ctrl["burnin"])),
  llk_per(as<int>(ctrl["log_post_interval"])),
  verbose(as<bool>(ctrl["verbose"])),
  theta(as<MatrixXd>(data["geo_race_table"])),
  geo_each_size(as<VectorXi>(data["voters_per_geo"])),
  Races(as< std::vector<IntegerVector> >(data["race_inits"])),
  check_in_sample(as<bool>(ctrl["fit_insample"]))
{
  
  //Initialize suff. stat for race count and race sample storage
  n_r.resize(R_);
  n_r.setZero();
  RaceSamples.resize(G_);
  for (int ii = 0; ii < G_; ++ii) { //iterate over geos
    (RaceSamples[ii]).resize(geo_each_size[ii], R_);
    (RaceSamples[ii]).setZero();
    for (int jj = 0; jj < geo_each_size[ii]; ++jj) { //iterate over names
      n_r(Races[ii][jj])++;
    }
  }
  //Construct list of name objects
  const List& all_name_data = as<List>(data["name_data"]);
  CharacterVector all_types = all_name_data.names();
  //XName name;
  for(int m = 0; m < M_; ++m){
    //name = XName(all_name_data[m], ctrl, G_, R_, n_r, Races, all_types[m]);
    names.emplace_back(XName(all_name_data[m], ctrl, G_, R_, n_r, Races, all_types[m]));
  }
  
  int c_val = 0, r_, w_;
  for (int ii = 0; ii < G_; ++ii) {
    geo_id_ = ii;
    geo_size = geo_each_size[geo_id_];
    // Iterate over each record in the geographic loc.
    for (int jj = 0; jj < geo_size; ++jj) {
      Rcpp::checkUserInterrupt();
      voter_ = jj;
      //Init name-specific suff stats
      for(int m = 0; m < M_; ++m){
        r_ = Races[ii][jj]; w_ = names[m].W[ii][jj];
        //initialize C (0 for non-keyword names, bern(0.7) otherwise)
        if(w_ < names[m].max_kw){
          c_val =  R::rbinom(1, 0.7);
          names[m].C[ii][jj] = c_val;
        }
        //Init n_rc suff. stat  matrix
        names[m].n_rc(r_, c_val)++;
        //Init n_wr suff. stat matrix
        if(!c_val){
          names[m].n_wr(w_, r_)++;
        }
      }
    }
  }
  
  
  
  // Initialize all placeholders
  geo_id_ = 0; geo_size = 0; w = 0; r = 0;
  c = 0; new_r = 0; voter_ = 0; N_ = 0; n_samp = 0;
  numerator = 0.0; denominator = 1.0;
  sum_r = 1.0; n_rc = 0.0;
  r_prob_vec.resize(R_);
  
  
  
  
  //If testing in sample fit
  if(check_in_sample){
    race_match.resize(G_);
    for (int ii = 0; ii < G_; ++ii) { //iterate over geos
      (race_match[ii]).resize(geo_each_size[ii]);
      for (int jj = 0; jj < geo_each_size[ii]; ++jj) { //iterate over names
        race_match[ii][jj] = 0;
      }
    }
    obs_race = as< std::vector<IntegerVector> >(data["race_obs"]);
  }

}

int keyWRU::sample_r(int voter,
                     int geo_id)
{
  // Extract current race
  r = Races[geo_id][voter];
  
  //remove data
  n_r(r)--;
  for(int m = 0; m < M_; ++m){
    names[m].n_rc(r, (names[m].C[geo_id])[voter])--;
    if(!(names[m].C[geo_id][voter])){
      names[m].n_wr((names[m].W[geo_id])[voter], r)--;
    }
  }
  
  for(int k = 0; k < R_; ++k){
    numerator = 0.0;
    denominator = 0.0;
    for(int m = 0; m < M_; ++m){
      const XName& name = names[m];
      w = (name.W[geo_id])[voter];
      c = (name.C[geo_id])[voter];
      n_rc = name.n_rc(k, c);
      numerator += log(n_rc + name.gamma_prior[c]) 
        + (c ? log(name.phi_tilde(w, k)) : log(name.n_wr(w, k) + name.beta_w)); 
      denominator += log(n_r(k) + name.gamma_prior.sum())
        + (c ? 0.0 : log(n_rc + ((double)(name.N_) * name.beta_w)));
    }
    r_prob_vec(k) = theta(geo_id, k) * exp(numerator - denominator);
  }
  sum_r = r_prob_vec.sum();
  new_r = rcat_without_normalize(r_prob_vec,
                                 sum_r,
                                 R_); // Cat(r_prob_vec/sum_r)
  
  
  //Add counts back in
  n_r(new_r)++ ;
  for(int m = 0; m < M_; ++m){
    names[m].n_rc(new_r, (names[m].C[geo_id])[voter])++ ;
    if(!(names[m].C[geo_id][voter])){
      names[m].n_wr((names[m].W[geo_id])[voter], new_r)++ ;
    }
  }
  
  
  return new_r;
}

void keyWRU::iteration_single(int it)
{ 
  //geo_indeces = shuffle_indeces(G_); // shuffle geo locs
  for (int ii = 0; ii < G_; ++ii) {
    geo_id_ = ii;//geo_indeces[ii];
    geo_size = geo_each_size[geo_id_];
    //record_indeces = shuffle_indeces(geo_size); //shuffle records
    // Iterate over each record in the geographic loc.
    for (int jj = 0; jj < geo_size; ++jj) {
      Rcpp::checkUserInterrupt();
      voter_ = jj;//record_indeces[jj];
      //Sample race
      Races[geo_id_][voter_] = sample_r(voter_, geo_id_);
      if((it+1) > burnin){
        //Store sampled race
        RaceSamples[geo_id_](voter_, Races[geo_id_][voter_])++;
        if(check_in_sample){
          race_match[geo_id_][voter_] += (Races[geo_id_][voter_] == obs_race[geo_id_][voter_]);
        }
      }
      //Sample mixture component
      for(int m = 0; m < M_; ++m){
        names[m].sample_c(Races[geo_id_][voter_], voter_, geo_id_);
      }
    }
  }
}


List keyWRU::return_obj()
{
  List phi_hat;
  MatrixXd phi_mat;
  for(int m = 0; m < M_; ++m){
    phi_mat = names[m].getPhiHat();
    phi_mat /= n_samp;
    phi_hat.push_back(phi_mat, names[m].type);
  }
  List res;
  res["phi"] = phi_hat;
  res["ll"] = model_fit;
  if(check_in_sample){
    res["r_insample"] = race_match;
  }
  res["predict_race"] = getRHat();
  return res;
}

void keyWRU::sample()
{
  // Set progress bar up
  Progress progress_bar(max_iter, verbose);
  
  for (int iter = 0; iter < max_iter; ++iter) {
    // Run iteration
    iteration_single(iter); 
    
    // Store samples and measures of model fit
    int r_index = iter + 1;
    if(r_index > burnin){
      if ((r_index % llk_per) == 0 || r_index == 1 || r_index == max_iter) {
        mfit_store();
      }
      if ((r_index % thin) == 0 || r_index == 1 || r_index == max_iter) {
        phihat_store();
        n_samp++;
      }
    }
    
    // Progress bar
    progress_bar.increment();
    
    // Check keybord interruption to cancel the iteration
    checkUserInterrupt();
  }
}

double keyWRU::loglik_total()
{
  double loglik = 0.0, beta_w;
  int N_;
  for (int r = 0; r < R_; ++r) {
    for( int m = 0; m < M_; ++m){
      loglik += mylgamma(names[m].n_rc(r, 1) + names[m].gamma_prior[0])
      + mylgamma(names[m].n_rc(r, 0) + names[m].gamma_prior[1]);
      loglik -= mylgamma(n_r[r] + names[m].gamma_prior.sum());
      N_ = names[m].N_;
      beta_w = names[m].beta_w;
      // Possible omp pragma here
      for(int w = 0; w < N_; ++w){
        loglik+=mylgamma(names[m].n_wr(w, r) + beta_w);
      }
      loglik -= mylgamma(names[m].n_rc(r, 0)
                           + ((double)(names[m].N_) * beta_w));
    }
  }
  return loglik;
}  



void keyWRU::mfit_store()
{
  // Store likelihood during the sampling
  double loglik = loglik_total();
  model_fit.push_back(loglik);
}

void keyWRU::phihat_store()
{
  // Store expected distribution over races for given name 
  for(int m = 0; m < M_; ++m){
    names[m].phihat_store();
  }
}

List keyWRU::getRHat()
{
  return(wrap(RaceSamples));  
}





