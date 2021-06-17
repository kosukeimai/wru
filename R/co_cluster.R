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
#'                    five characters. See \code{\link{predict_race}} for other optional fields.
#' @param name_types  Character vector. Must have elements in "surname", "first", and "middle".
#' @param name_race_tables Named list, with as many elements as there are names in \code{name_types}, and names matching
#'                         elements in \code{name_types}. Each list element should be a data.frame of unique names (first column) by race (remaining columns),
#'                         with conditional probabilities p(Name|Race). 
#' @param census_geo Required character vector. One of "county", "tract", "block" or "place". See \code{\link{predict_race}}.                        
#' @param ... Arguments passed to \code{\link{predict_race}}.
#' @param control List of control arguments, including 
#' \itemize{
#' \item{race_init}{ Initial race for each observation in voter.file. Must be an integer, with
#'                  0=white, 1=black, 2=hispanic, 3=asian, and 4=other.}
#'  \item{fit_insample}{ Boolean. Should model check in-sample fit of race prediction for each
#'                      record? If \code{TRUE}, \code{race_obs} cannot be \code{NULL}. Defaults to \code{FALSE}.}
#'  \item{race_obs}{ Observed race for each record in \code{voter.file}. Must be an integer, with
#'                  0=white, 1=black, 2=hispanic, 3=asian, and 4=other.}
#'  \item{iter}{ Number of MCMC iterations. Defaults to 1000.}
#'  \item{burnin}{ Number of iterations discarded as burnin. Defaults to half of \code{iter}.}
#'  \item{thin}{ Thinning interval for MCMC. Defaults to 1.}
#'  \item{log_post_interval}{ Interval for storing the log_posterior. Defaults to 10.}
#'  \item{beta_prior}{ Parameter for symmetric Dirichlet prior over names for each race. Defaults to 5.}
#'  \item{gamma_prior}{ Parameter for Beta prior over keyname/non-keyname mixture components. Defaults to c(5, 5).}
#'  \item{verbose}{ Print progress information. Defaults to \code{TRUE}.}
#'  \item{seed}{ RNG seed. If \code{NULL}, a seed is generated and stored for reproducibility.}
#' }
#'
#'
#' @return A named list:
#' \itemize{
#' \item{name_by_race}{ Named list of predicted distributions of name by race for each name type.}
#' \item{race_by_record}{ A copy of \code{voter.file}, with additional columns of predicted
#'                       race probabilities, names \code{pred.<race>}. }
#' \item{loglik}{ Values of log likelihood, evaluated every \code{log_post_interval}.}
#' \item{fit_insample}{ When \code{fit_insample=TRUE}, a probability of correct in_sample prediction 
#'                      for each record in \code{vote.file}.} 
#' } 
#' 
#' 
#' @export
#'
co_cluster <- function(voter.file,
                       name_types,
                       name_race_tables,
                       census.geo,
                       ...,
                       control = NULL)
{
  ##Data quality checks
  n_race <- ncol(name_race_tables[[1]])-1
  stopifnot(all(sapply(name_race_tables, ncol) == n_race+1),
            all(name_types %in% c("surname","first", "middle")),
            all(names(name_race_tables) %in% name_types),
            name_types %in% names(voter.file),
            census.geo %in% c("county","tract","block","place")
            )
  
  ## Form control list
  ctrl <- list(iter = 1000,
               thin = 1,
               race_init = NULL,
               log_post_interval = 10,
               beta_prior = 5,
               gamma_prior = c(5, 5),
               verbose = TRUE, 
               fit_insample = FALSE,
               race_obs = NULL,
               max_keynames = 1000,
               seed = sample(1:1000, 1))
  ctrl$burnin <- floor(ctrl$iter/2)
  ctrl[names(control)] <- control
  
  ## Set RNG seed
  set.seed(ctrl$seed)
  
  if(ctrl$fit_insample){
    stopifnot(!is.null(ctrl$race_obs))
  }
  
  ## Initial race 
  race_pred_args <- list(census.surname = TRUE,
                         surname.only = FALSE,
                         surname.year = 2010,
                         census.geo = census.geo,
                         age = FALSE,
                         sex = FALSE,
                         retry = 0)
  args_usr <- list(...)
  ## level of geo aggregation
  geo_id_names <- c("state", switch(race_pred_args$census.geo,
                                    "county" = c("county"),
                                    "tract" = c("county","tract"),
                                    "block" = c("county","tract","block"),
                                    "place" = c("place"),
                                    "zip" = c("zip")))
  
  race_pred_args[names(args_usr)] <- args_usr
  if(is.null(race_pred_args$census.data)){
    if(is.null(race_pred_args$census.key)){
      stop("Geographic data is required. When `census.data' is NULL, you must provide a census API Key using `census.key' so I can download the required data.")
    }
    all_states <- unique(voter.file$state)
    race_pred_args$census.data <- get_census_data(race_pred_args$census.key, 
                                   all_states, 
                                   race_pred_args$age,
                                   race_pred_args$sex, 
                                   race_pred_args$census.geo, 
                                   race_pred_args$retry)
  }
  
  race.suff <- c("whi", "bla", "his", "asi", "oth")
  if(is.null(ctrl$race_init)){
    race_pred_args$voter.file <- voter.file
    race_pred <- do.call(predict_race, race_pred_args)
    ctrl$race_init <- apply(race_pred[,paste0("pred.",race.suff)], 1, which.max) - 1 
  }
  geo_id <- do.call(paste, voter.file[,geo_id_names])
  ctrl$race_init <- split(ctrl$race_init, geo_id)
 
  
  
  ## P(race | geo)
  g_r_t <- do.call(rbind, lapply(race_pred_args$census.data, 
                                 function(x){
                                   all_names <- names(x[[race_pred_args$census.geo]])
                                   tmp <- x[[race_pred_args$census.geo]][,c(geo_id_names, grep("P00", all_names, value=TRUE))]
                                   tmp[,grep("P00", all_names, value=TRUE)] <- proportions(as.matrix(tmp[,grep("P00", all_names, value=TRUE)]), 1)
                                   tmp$r_whi <- tmp$P005003  #Pr(White|Geo)
                                   tmp$r_bla <- tmp$P005004  #Pr(Black|Geo)
                                   tmp$r_his <- tmp$P005010  #Pr(Latino|Geo)
                                   tmp$r_asi <- (tmp$P005006 + tmp$P005007) #Pr(Asian or NH/PI|Geo)
                                   tmp$r_oth <- (tmp$P005005 + tmp$P005008 + tmp$P005009)#Pr(AI/AN, Other, or Mixed|Geo)
                                   return(tmp)
                                 }))
  g_r_t_geo <- do.call(paste, g_r_t[,geo_id_names])
  ##Subset to geo's in vf
  g_r_t <- g_r_t[g_r_t_geo %in% geo_id, ]
  g_r_t_geo_new <- do.call(paste, g_r_t[,geo_id_names])
  geo_ord <- match(names(ctrl$race_init),g_r_t_geo_new)
  geo_race_table <- as.matrix(g_r_t[geo_ord,grep("r_", names(g_r_t))])
  
  
  ##Name-specific data
  name_data <- vector("list", length(name_types))
  names(name_data) <- name_types 
  for(ntype in name_types){
    str_names <- toupper(name_race_tables[[ntype]][,1])
    proc_names_str <- .name_preproc(voter.file[,ntype], c(str_names))
    u_obs_names <- unique(proc_names_str)
    keynames_str_all <- str_names
    kw_in_ind <- keynames_str_all %in% proc_names_str
    keynames_str <- keynames_str_all[kw_in_ind]
    u_kw <- unique(keynames_str)
    n_u_kw <- length(u_kw)
    reord <- order(match(u_obs_names, u_kw))
    u_obs_names <- u_obs_names[reord]
    n_names <- length(u_obs_names)
    w_names <- match(proc_names_str, u_obs_names) - 1
    keynames <- match(keynames_str, table = u_kw) - 1 
    w_names_list <- split(w_names, geo_id)
    phi_tilde <- array(0.0, c(n_u_kw, n_race))
    for(x in 1:n_race){
      phi_tilde[, x] <- proportions(name_race_tables[[ntype]][which(kw_in_ind),x+1]) 
    }
    
    colnames(phi_tilde) <- paste("p", race.suff, sep="_")
    name_data[[ntype]] <- list(n_unique_names = n_names,
                               record_name_id = w_names_list,
                               largest_keyword = as.integer(max(keynames)),
                               census_table = phi_tilde,
                               beta_prior = ctrl$beta_prior,
                               gamma_prior = ctrl$gamma_prior,
                               u_obs_names = u_obs_names)
  }
  
  ## Create data for keyWRU
  data_list <- list(name_type_n = length(name_types),
                    race_n = ncol(name_race_tables[[1]][,-1]),
                    geo_n = length(unique(geo_id)),
                    geo_race_table = geo_race_table,
                    voters_per_geo = sapply(split(voter.file, geo_id), nrow), 
                    race_inits = ctrl$race_init,
                    race_obs = if(ctrl$fit_insample){split(ctrl$race_obs, geo_id)} else {list()},
                    name_data = name_data)
  
  full_res <- keyWRU_fit(data_list, ctrl)
  pred_list <- full_res$phi
  res <- lapply(name_types, 
                function(ntype){
                  colnames(pred_list[[ntype]]) <- paste0("pred.",race.suff)
                  pred_list[[ntype]] <- as.data.frame(pred_list[[ntype]])
                  pred_list[[ntype]] <- cbind(name_data[[ntype]]$u_obs_names,
                                              pred_list[[ntype]])
                  names(pred_list[[ntype]])[1] <- ntype
                  return(pred_list[[ntype]])
                })
  v.file.s <- split(voter.file, geo_id)
  race_samp <- cbind(do.call(rbind, v.file.s),
                     proportions(as.matrix(do.call(rbind, full_res$predict_race)), 1))
  names(race_samp) <- c(names(voter.file), paste0("pred.",race.suff))
  names(res) <- names(pred_list)
  ret_obj <- list()
  ret_obj$name_by_race <- res
  ret_obj$race_by_record <- race_samp
  ret_obj$loglik <- full_res$ll
  if(ctrl$fit_insample){
    ret_obj$fit_insample <- do.call(c,full_res$r_insample)/(ctrl$iter - ctrl$burnin)
  }
  return(ret_obj)
}