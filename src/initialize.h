#ifndef __Initialize__INCLUDED__
#define __Initialize__INCLUDED__

#include <Rcpp.h>
#include <RcppEigen.h>
#include <string>
#include <unordered_set>
#include <unordered_map>
#include "sampler.h"

using namespace Eigen;
using namespace Rcpp;
using namespace std;


class keyATMinitialize
{
  public:
    //
    // Data
    //
    List docs;
    List info;
    List initialized;
    List keywords_raw;

    int model_key;
    int keyword_k;
    int total_k;
    int doc_num;
    int doc_len;

    CharacterVector wd_names;
    int vocab_size;

    // Word ids
    unordered_map<string, int> wd_map;

    // Keywords
    unordered_set<int> keywords_set;
    int keywords_num;

    unordered_map<int, vector<int>> key_topic_map;
    unordered_map<int, int> key_count_map;


    //
    // Initialize objects
    //
    List W;
    List Z;
    List S;
    List keywords_id;


    //
    // Constructor and Deconstructor
    //
    keyATMinitialize(List docs_, List info_,  List initialized_);
    ~keyATMinitialize();

  
    //
    // Main functions
    //
    void data_load();

    void initialize_keyATM();
      void initialize_keywords();
    void initialize_LDA();


    //
    // Utilities
    //

    // Return model
    List return_initialized();

};


#endif
