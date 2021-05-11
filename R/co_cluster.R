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
#'                         elements in \code{name_types}. Each list element should be a data.frame of unique names (first column) by race (remaining columns),
#'                         with conditional probabilities p(Race|Name). Defaults to 
#'                         \code{list(surname = wru::surnames2010)}.    
#' @param census.data A list indexed by two-letter state abbreviations, which contains pre-saved Census geographic data. 
#'                       Can be generated using \code{census_geo_api_joint} function. Defaults to \code{NULL}, which 
#'                       calls that function. 
#' @param key_method  Method for extracting keynames from name race tables. Currently, only "mutual.inf" is implemented. 
#' @param ... Arguments passed to \code{predict_race}.
#' @param control List of control arguments, including 
#' \itemize{
#' \item{race_init}{ Initial race for each observation in voter.file. Must be an integer, with
#'                  0=white, 1=black, 2=hispanic, 3=asian, and 4=other.}
#'  \item{max_keynames}{ Maximum number of names in census table to use as keynames. Defaults to 100.}
#'  \item{fit_insample}{ Boolean. Should model check in-sample fit of race prediction for each
#'                      record? If \code{TRUE}, \code{race_obs} cannot be \code{NULL}. Defaults to \code{FALSE}.}
#'  \item{race_obs}{ Observed race for each observation in voter.file. Must be an integer, with
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
                       control = NULL)
{
  ##Data quality checks
  n_race <- 5
  stopifnot(all(sapply(name_race_tables, ncol) == n_race+1),
            all(name_types %in% c("surname","first", "middle")),
            all(names(name_race_tables) %in% name_types),
            key_method %in% c("mutual.inf"),
            name_types %in% names(voter.file))
  
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
                         age = FALSE,
                         sex = FALSE,
                         retry = 0)
  args_usr <- list(...)
  race_pred_args[names(args_usr)] <- args_usr
  
  race_pred_args$voter.file <- voter.file
  if(!is.null(census.data)){
    recurse <- function (L, f) {  
      if (inherits(L, "data.frame")){
        f(L) 
      } else if (!is.list(L) && length(L) == 1) {
        return(L)
      } else {
        lapply(L, recurse, f) 
      }
    } 
    race_pred_args$census.data <- census.data
    race_pred_args$census.data <- recurse(race_pred_args$census.data,
                                         f = function(x){
                                           ind <- grep("r_", names(x))
                                           x[,ind] <- proportions(as.matrix(x[,ind]), 2)
                                           return(x)
                                         })
  }
  race.suff <- c("whi","bla","his", "asi", "oth")
  if(is.null(ctrl$race_init)){
    race_pred <- do.call(predict_race, race_pred_args)
    ctrl$race_init <- apply(race_pred[,paste0("pred.",race.suff)], 1, which.max) - 1 
  }
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
    census.data <- get_census_data(race_pred_args$census.key,
                                   states,
                                   race_pred_args$age,
                                   race_pred_args$sex,
                                   race_pred_args$census.geo,
                                   race_pred_args$retry,
                                   joint = TRUE)
  }
  g_r_t <- do.call(rbind, lapply(census.data, 
                                 function(x){
                                   all_names <- names(x[[race_pred_args$census.geo]])
                                   x[[race_pred_args$census.geo]][,c(geo_id_names, grep("r_", all_names, value=TRUE))]
                                 }))
  g_r_t_geo <- do.call(paste, g_r_t[,geo_id_names])
  geo_race_table <- proportions(as.matrix(g_r_t[,grep("r_", names(g_r_t))]), 1)
  
  
  ##Reorder voterfile data to match census
  geo_id <- do.call(paste, voter.file[,geo_id_names])
  voter.file.reord <- voter.file[order(match(geo_id, g_r_t_geo)),]
  geo_id <- do.call(paste, voter.file.reord[,geo_id_names])
  
  
  ##Name-specific data
  name_data <- vector("list", length(name_types))
  names(name_data) <- name_types 
  for(ntype in name_types){
    str_names <- name_race_tables[[ntype]][,1]
    proc_names_str <- .name_preproc(voter.file[,ntype], c(str_names))
    u_obs_names <- unique(proc_names_str)
    keynames_str <- .find_keynames(str_names,
                                   as.matrix(name_race_tables[[ntype]][,-1]),
                                   u_obs_names,
                                   key_method,
                                   ctrl$max_keynames)
    dist_keynames <- lapply(seq.int(n_race), 
                            function(x){
                              tmp <- name_race_tables[[ntype]][str_names %in% keynames_str[[x]],]
                              tmp[,-1] <- proportions(as.matrix(tmp[,-1]), 2)
                              return(tmp[,c(1,x+1)])
                            })
    u_kw <- unique(unlist(keynames_str))
    n_u_kw <- length(u_kw)
    reord <- order(match(u_obs_names, u_kw))
    u_obs_names <- u_obs_names[reord]
    n_names <- length(u_obs_names)
    w_names <- match(proc_names_str, u_obs_names) - 1
    keynames <- lapply(keynames_str, 
                       FUN=function(x){
                         match(x, table = u_kw) - 1
                       })
    w_names_list <- split(w_names, geo_id)
    phi_tilde <- array(0.0, c(n_u_kw, n_race))
    for(x in 1:n_race){
      phi_tilde[match(dist_keynames[[x]][,1], table=u_kw),x] <- dist_keynames[[x]][,2] 
    }
    
    colnames(phi_tilde) <- paste("p", race.suff, sep="_")
    name_data[[ntype]] <- list(n_unique_names = n_names,
                               record_name_id = w_names_list,
                               keynames = keynames,
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
                    race_inits = split(ctrl$race_init, geo_id),
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
  names(res) <- names(pred_list)
  ret_obj <- list()
  ret_obj$name_by_race <- res
  ret_obj$loglik <- full_res$ll
  if(ctrl$fit_insample){
    ret_obj$fit_insample <- do.call(c,full_res$r_insample)/length(ret_obj$loglik)
  }
  return(ret_obj)
}