#' Name-Race Co-clustering Using Keynames
#' 
#' Estimate a Bayesian mixed-membership, keyword-assisted race classification model for geo-locations
#' in a voterfile.  
#'
#' @param voter.file 	An object of class data.frame. Must contain a row for each individual being predicted, 
#'                    as well as fields named "surname", and/or"first", and/or "middle", containing each 
#'                    individual's corresponding names. It must also contain a field named state, which 
#'                    contains the two-character lower-case abbreviation for each individual's state of residence (e.g., "nj" for New Jersey).
#'                    If using Census geographic data in race/ethnicity
#'                    predictions, voter.file must also contain at least one of the following fields: county, tract, block,
#'                    and/or place. These fields should contain character strings matching U.S. Census categories. County is three
#'                    characters (e.g., "031" not "31"), tract is six characters, and block is four characters. Place is
#'                    five characters. See below for other optional fields.
#' @param name_types  Character vector. Must have elements in "surname", "first", and "middle". Defaults to "surname". 
#' @param name_race_tables Named list, with as many elements as there are names in \code{name_types}, and names matching
#'                         elements in \code{name_types}. Each list element should be a matrix of unique names (rows) by race (cols),
#'                         with conditional probabilities p(race|name) (i.e. each matrix should be row-stochastic.). Defaults to 
#'                         \code{list(surname = wru::surnames2010)}.    
#' @param census.data A list indexed by two-letter state abbreviations, which contains pre-saved Census geographic data. 
#'                       Can be generated using \code{census_geo_api_joint} function. Defaults to \code{NULL}, which 
#'                       calls that function. 
#' @param key_method  Method for extracting keynames from name race tables. Currently, only "mutual.inf" is implemented. 
#' @param ... Arguments passed to \code{predict_race}.
#' @param control List of control arguments, including 
#' \itemize{
#'  \item{n_keynames}{Number of names in census table to use as keynames. Defaults to 100.}
#'  \item{iter}{Number of MCMC iterations. Defaults to 1000.}
#'  \item{burnin}{Number of iterations discarded as burnin. Defaults to half of \code{iter}.}
#'  \item{thin}{Thinning interval for MCMC. Defaults to 1.}
#'  \item{log_posterior_interval}{Interval for storing the log_posterior. Defaults to 10.}
#'  \item{beta_prior}{Parameter for symmetric Dirichlet prior over names for each race. Defaults to 5.}
#'  \item{gamma_prior}{Parameter for Beta prior over keyname/non-keyname mixture components. Defaults to c(5, 5).}
#'  \item{verbose}{Print progress information. Defaults to \code{TRUE}.}
#'  \item{seed}{RNG seed. If \code{NULL}, a seed is generated and stored for reproducibility.}
#' }
#'
#'
#' @return A named list of predicted distributions over races for each
#' name type. 
#' 
#' @export
#'
co_cluster <- function(voter.file,
                       name_types = c("surname"),
                       name_race_tables = list(surname = wru::surnames2010),
                       census.data = NULL,
                       key_method = "mutual.inf",
                       ...,
                       control)
{
  ##Data quality checks
  stopifnot(all.equal(lapply(name_race_tables, ncol)),
            all(name_types %in% c("surname","first", "middle")),
            key_method %in% c("mutual.inf"),
            name_types %in% names(voter.file))
  
  ## Form control list
  ctrl <- list(iter = 1000,
               thin = 1,
               log_posterior_interval = 10,
               beta_prior = 5,
               gamma_prior = c(5, 5),
               verbose = TRUE, 
               n_keynames = 100,
               seed = sample(1:1000, 1))
  ctrl$burnin <- floor(ctrl$iter/2)
  ctrl[names(control)] <- control
  
  ## Set RNG seed
  set.seed(ctrl$seed)
  
  ## Initial race 
  race_pred_args <- list(...)
  race_pred_args$voter.file <- voter.file
  race_pred <- do.call(predict_race, race_pred_args)
  race.suff <- c("whi","bla","his", "asi", "oth")
  race_inits_int <- apply(race_pred[,paste0("pred.",race.suff)], 1, which.max - 1)
  
  ## level of geo aggregation
  geo_id_names <- c("state", switch(race_pred_args$census.geo,
                         "county" = c("county"),
                         "tract" = c("county","tract"),
                         "block" = c("county","tract","block"),
                         "place" = c("place"),
                         "zip" = c("zip")))
  
  
  ## P(race | geo)
  if(is.null(census.data)){
    if(is.null(race_pred_args$census.key)){
      stop("If no census.data is provided, you must enter U.S. Census API key, which can be requested at https://api.census.gov/data/key_signup.html.")
    }
    states <- unique(voter.file[,"state"])
    census.data <- census_geo_api_joint(race_pred_args$census.key,
                                  unique(voter.file[,"state"]),
                                  race_pred_args$census.geo,
                                  race_pred_args$age,
                                  race_pred_args$sex,
                                  race_pred_args$retry)
  }
  g_r_t <- do.call(rbind, lapply(census.data, 
                                 function(x){
                                   all_names <- names(x[[race_pred_args$census.geo]])
                                   x[[race_pred_args$census.geo]][,c(geo_id_names, grep("r_", all_names))]
                                 }))
  g_r_t_geo <- do.call(paste, g_r_t[,geo_id_names])
  geo_race_table <- proportions(g_r_t[,grep("r_", names(g_r_t))], 1)
  
  
  ##Reorder voterfile data to match census
  geo_id <- do.call(paste, voter.file.reord[,geo_id_names])
  voter.file.reord <- voter.file[order(match(geo_id, g_r_t_geo)),]
  geo_id <- do.call(paste, voter.file.reord[,geo_id_names])
  
  
  ##Name-specific data
  name_data <- vector("list", length(name_types))
  names(name_data) <- name_types 
  for(ntype in name_types){
    str_names <- name_race_tables[[ntype]][,1]
    keynames_str <- .find_keynames(str_names,
                                   as.matrix(name_race_tables[[ntype]][,-1]),
                                   key_method,
                                   ctrl$n_keynames)
    proc_names_str <- .name_preproc(voter.file[,ntype], c(keynames_str))
    u_kw <- unique(c(keynames_str))
    u_names <- unique(proc_names_str)
    reord <- order(match(u_names, u_kw))
    u_names <- u_names[reord]
    n_names <- length(u_names)
    w_names <- match(proc_names_str, u_names) - 1
    keynames <- apply(keynames_str, 2, match, table = u_names) - 1
    w_names_list <- split(w_names, geo_id)
    phi_tilde_freq <- name_race_tables[[ntype]][match(str_names, u_kw),]
    phi_tilde <- proportions(phi_tilde_freq,1)
    name_data[ntype] <- list(n_unique_names = n_names,
                             record_name_id = w_names_list,
                             keynames = keynames,
                             census_table = phi_tilde,
                             beta_prior = ctrl$beta_prior,
                             gamma_prior = ctrl$gamma_prior)
  }
  
  ## Create data for keyWRU
  data_list <- list(name_type_n = length(name_types),
                    race_n = ncol(name_race_tables[[1]]),
                    geo_n = length(unique(geo_id)),
                    geo_race_table = geo_race_table,
                    voters_per_geo = length(split(voter.file, geo_id)), 
                    race_inits = split(race_inits_int, geo_id),
                    name_data = name_data)
  
  res <- keyWRU_fit(data_list, ctrl)
  return(res)
}