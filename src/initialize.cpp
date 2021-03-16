#include <Rcpp.h>
#include <RcppEigen.h>
#include <iostream>
#include <algorithm>
#include <string>
#include <unordered_set>
#include <unordered_map>

// Header files
#include "sampler.h"
#include "initialize.h"

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppEigen)]]


using namespace Eigen;
using namespace Rcpp;
using namespace std;



//
// Functions
//
keyATMinitialize::keyATMinitialize(List docs_, List info_, List initialized_)
{
  // Constructor
  docs = docs_;
  info = info_;
  initialized = initialized_;

  // Load
  data_load();

  // Initialize
  if (model_key) {
    initialize_keyATM(); 
  } else {
    initialize_LDA(); 
  }
}


keyATMinitialize::~keyATMinitialize()
{
  // Destructor
}


List keyATMinitialize::return_initialized()
{
  initialized["W"] = W;
  initialized["Z"] = Z;

  if (model_key) {
    // keyATM 
    initialized["S"] = S;
    initialized["keywords_id"] = keywords_id;
  }

  return initialized;
}



//
// Main functions
//
void keyATMinitialize::data_load()
{
  keyword_k = info["keyword_k"];
  total_k = info["total_k"];

  doc_num = docs.size();

  model_key = initialized["model_key"];
  W = initialized["W"];
  Z = initialized["Z"];

  if (model_key) {
    // keyATM 
    S = initialized["S"];
    keywords_id = initialized["keywords_id"];
  } 

  // Load vocabulary
  wd_names = info["wd_names"];
  vocab_size = wd_names.size();
  string word;

  for (int wid = 0; wid < vocab_size; wid++) {
    word = wd_names[wid];
    wd_map[word] = wid; 
  }
}



void keyATMinitialize::initialize_keyATM()
{
  initialize_keywords();

  CharacterVector doc;
  string word;
  int wid;
  int doc_len;
  int z;
  int s;

  int keyword_num_appear;

  double u;
  double prob;
  double prob_allK = 1.0 / total_k;
  int index;
  
  for (int doc_id = 0; doc_id < doc_num; doc_id++) {
    doc = docs[doc_id];
    doc_len = doc.size();
  
    IntegerVector W_doc = W[doc_id];
    IntegerVector Z_doc = Z[doc_id];
    IntegerVector S_doc = S[doc_id];
  
    for (int doc_pos = 0; doc_pos < doc_len; doc_pos++) {
      // W 
      word = doc[doc_pos];
      wid = wd_map[word];
      W_doc[doc_pos] = wid;
  
      // Z and S
      if (keywords_set.find(wid) != keywords_set.end()) {
        // Keyword
        keyword_num_appear = key_count_map[wid];

        // Z
        if (keyword_num_appear == 1) {
          z =  key_topic_map[wid][0];
        } else {
          index = sampler::rcat_eqsize(keyword_num_appear);
          z = key_topic_map[wid][index];
        }

        // S
        u = R::unif_rand();
        if (u < 0.3) {
          s = 0; 
        } else {
          s = 1;  
        }
      } else {
        // Not a keyword
        z = sampler::rcat_eqprob(prob_allK, total_k);
        s = 0;
      }

      Z_doc[doc_pos] = z;
      S_doc[doc_pos] = s;
    }
  
    W[doc_id] = W_doc;
    Z[doc_id] = Z_doc;
    S[doc_id] = S_doc;
  }

}


void keyATMinitialize::initialize_keywords()
{
  // key_topic_map: map keyword id to topic
  // key_count_map: map keyword id to the number of topics it appears

  keywords_raw = info["keywords_raw"];
  CharacterVector keywords_topic;
  string keyword;
  int keyword_id;

  for (int k = 0; k < keywords_raw.size(); k++) {
    keywords_topic = keywords_raw[k]; 
    IntegerVector keywords_id_k = keywords_id[k];

    for (int j = 0; j < keywords_topic.size(); j++) {
      keyword = keywords_topic[j];
      keyword_id = wd_map[keyword]; 
      keywords_id_k[j] = wd_map[keyword];

      if (keywords_set.find(keyword_id) != keywords_set.end()) {
        // Already registered 
        key_topic_map[keyword_id].push_back(k);
        key_count_map[keyword_id] += 1;
      } else {
        // New keyword 
        keywords_set.insert(keyword_id);
        vector<int> topics;
        topics.push_back(k);
        key_topic_map[keyword_id] = topics;

        key_count_map[keyword_id] = 1;
      }
    }

    keywords_id[k] = keywords_id_k;
  }
   
}


void keyATMinitialize::initialize_LDA()
{
  CharacterVector doc;
  string word;
  int z;
  int wid;
  int doc_len;
  double prob = 1.0 / total_k;

  for (int doc_id = 0; doc_id < doc_num; doc_id++) {
    doc = docs[doc_id];
    doc_len = doc.size();

    IntegerVector W_doc = W[doc_id];
    IntegerVector Z_doc = Z[doc_id];
    IntegerVector S_doc = S[doc_id];

    for (int doc_pos = 0; doc_pos < doc_len; doc_pos++) {
      word = doc[doc_pos];
      wid = wd_map[word];
      W_doc[doc_pos] = wid;

      z = sampler::rcat_eqprob(prob, total_k);    
      Z_doc[doc_pos] = z;
    }

    W[doc_id] = W_doc;
    Z[doc_id] = Z_doc;
  }

}


//
// Call from R
//

//' Initialize assignments
//'
//' @param info Various information
//' @param initialized Store initialized objects (W, S and Z) 
//'
//' @keywords internal
// [[Rcpp::export]]
List make_wsz_cpp(List docs_, List info_, List initialized_)
{
  keyATMinitialize init_obj(docs_, info_, initialized_);
  initialized_ = init_obj.return_initialized();
  return initialized_;
}


