#' Race prediction function, for hierarchical WRU model
#' 
#' Sample individual-level race/ethnicity from a hirarchical Bayesian Dirichlet-multinomial model.
#'
#' @param voter.file 	An object of class data.frame. Must contain a row for each individual being predicted, 
#'                    as well as fields named "surname", and/or"first", and/or "middle", containing each 
#'                    individual's corresponding names. It must also contain a field named state, which 
#'                    contains the two-character lower-case abbreviation for each individual's state of residence (e.g., "nj" for New Jersey).
#'                    voter.file must also contain a field with county codes and any additional levels (i.e. tract, block) for lower levels of aggregation.
#'                    These fields should contain character strings matching U.S. Census categories. County is three
#'                    characters (e.g., "031" not "31"), tract is six characters, and block is four characters. Place is
#'                    five characters. See \code{\link{predict_race}} for other optional fields.
#' @param namesToUse  Character string Must be one of 'last', 'last, first', or 'last, first, middle'.
#' @param census.geo Required string. One of "county", "tract", "block" or "place". See \code{\link{predict_race}}.       
#' @param geo.cluster Required string. Higher level of aggregation for hierarchical model. One of "county", "zipcode", or "tract". 
#'                    Must be a higher level of aggregation than \code{census.geo}. Defaults to "county". See Details for more information.                     
#' @param me.correct Boolean. Should the model use a hierarchical model for correcting measurement error?                    
#' @param race.init Initial race for each observation in voter.file. Must be an integer, with
#'                  0=white, 1=black, 2=hispanic, 3=asian, and 4=other. Defaults to values obtained from \code{race_predict}.
#' @param ... Other arguments passed to \code{\link{predict_race}}.
#' @param control List of control arguments, including 
#' \itemize{
#'  \item{iter}{ Number of MCMC iterations. Defaults to 1000.}
#'  \item{burnin}{ Number of iterations discarded as burnin. Defaults to half of \code{iter}.}
#'  \item{verbose}{ Print progress information. Defaults to \code{TRUE}.}
#'  \item{me.correct}{ Boolean. Should the model use a hierarchical model for correcting measurement error? Defaults to \code{TRUE}.}
#'  \item{progress_bar}{ If \code{verbose=TRUE}, should a progress bar be used to track progress? If \code{TRUE}, must be handled by \code{progressr::handlers()},
#'                       as per \code{progressr}'s design choice.}
#'  \item{seed}{ RNG seed. If \code{NULL}, a seed is generated and returned for reproducibility.}
#' }
#'
#'
#' @return A named list:
#' \itemize{
#' \item{race_by_record}{ A copy of \code{voter.file}, with additional columns of predicted
#'                       race probabilities, names \code{pred.<race>}. }
#'  \item{seed}{ RNG seed used.}                     
#' } 
#' 
#' @importFrom foreach foreach "%dopar%" getDoParWorkers
#' @export
#'
predict_race_hwru <- function(voter.file,
                              namesToUse,
                              census.geo,
                              census.names = NULL,
                              geo.cluster = "county",
                              race.init = NULL,
                              ...,
                              control = NULL)
{
  ##Preliminary Data quality checks
  .hasData()
  n_race <- 5
  if(!(namesToUse %in% c(c("last"),c("last, first"), c("last, first, middle")))){
    stop("'namesToUse' must be one of 'last', 'last, first', or 'last, first, middle'")
  }
  name_types <- gsub(" ", "", strsplit(namesToUse,",")[[1]])
  if(!all(name_types %in% names(voter.file))){
    stop("When used, 'last', 'first', and 'middle' must be variable names in voter.file.")
  }
  ## Other quick checks...
  stopifnot(census.geo %in% c("county","tract","block","place"),
            all(!is.na(voter.file$last)))
  stopifnot(geo.cluster %in% c("county","tract","zipcode"),
            all(!is.na(voter.file$last)))
  if(!(geo.cluster %in% colnames(voter.file))){
    stop(sprintf("For the hierarchical estimation, I need information on each record's %s", geo.cluster))
  }
  
  orig.names <- names(voter.file)
  orig.state <- voter.file$state
  voter.file$state <- toupper(voter.file$state)
  voter.file$rec_id_ <- 1:nrow(voter.file)
  
  ## Form control list
  ctrl <- list(iter = 1000,
               thin = 1,
               verbose = TRUE, 
               lambda = c(0.638, 0.121, 0.164, 0.048, 0.029),
               parallel = TRUE,
               me.correct = TRUE,
               census.dict = FALSE,
               seed = sample(1:1000, 1),
               threads=2)
  ctrl$burnin <- floor(ctrl$iter/2)
  ctrl[names(control)] <- control
  
  ## Set RNG seed
  set.seed(ctrl$seed)
  
  ## Initial race 
  race_pred_args <- list(census.data = NULL,
                         census.geo = census.geo,
                         census.key = NULL,
                         namesToUse = namesToUse,
                         retry = 0
                         )
  args_usr <- list(...)
  ## level of geo estimation
  geo_id_names <- c("state", switch(race_pred_args$census.geo,
                                    "county" = c("county"),
                                    "tract" = c("county","tract"),
                                    "block" = c("county","tract","block"),
                                    "place" = c("place"),
                                    "zipcode" = c("zipcode")))
  
  
  race_pred_args[names(args_usr)] <- args_usr
  all_states <- unique(voter.file$state)
  if(is.null(race_pred_args$census.data)){
    if(is.null(race_pred_args$census.key)){
      stop("Geographic data is required. When `census.data' is NULL, you must provide a census API Key using `census.key' so I can download the required data.")
    }
    race_pred_args$census.data <- get_census_data(race_pred_args$census.key, 
                                                  all_states, 
                                                  race_pred_args$census.geo, 
                                                  race_pred_args$retry)
  } else {
    race_pred_args$census.data <- race_pred_args$census.data[all_states]
  }
  race.suff <- c("whi", "bla", "his", "asi", "oth")
  if(is.null(race.init)){
    if(ctrl$verbose){
      cat("Using predict_race() to obtain initial race predictions...\n")
    }
    race_pred_args$voter.file <- voter.file
    race_pred <- do.call(predict_race_new, race_pred_args)
    race.init <- apply(race_pred[,paste0("pred_",race.suff)], 1, which.max) 
    rm(race_pred)
  }
  if(any(is.na(race.init))){
    stop("Some initial race values are NA.\n
         If you didn't provide initial values, check the results of calling predict_race() on the voter.file you want me to work on.\n
         The most likely reason for getting a missing race prediction is having a missing geolocation value.")
  }
  geo_id <- do.call(paste, voter.file[,geo_id_names])
  
  if(ctrl$verbose){
    cat("Forming Pr(race | location) tables from census data...\n")
  }
  tmp_tabs <- lapply(race_pred_args$census.data, 
                     function(x){
                       all_names <- names(x[[race_pred_args$census.geo]])
                       tmp <- x[[race_pred_args$census.geo]][,c(geo_id_names, grep("P00", all_names, value=TRUE))]
                       tmp$r_whi <- tmp$P005003  
                       tmp$r_bla <- tmp$P005004  
                       tmp$r_his <- tmp$P005010  
                       tmp$r_asi <- (tmp$P005006 + tmp$P005007) 
                       tmp$r_oth <- (tmp$P005005 + tmp$P005008 + tmp$P005009)
                       tmp$ccc <- 1:nrow(tmp)
                       all_names <- names(tmp)
                       ## Totals
                       n_rc <- simplify2array(by(tmp[,grep("r_", names(tmp))], tmp[,geo.cluster], colSums))
                       colnames(n_rc) <- paste0(tmp$state[1], colnames(n_rc))
                       ## Probabilities
                       tmp_l <- tmp[,c("ccc",grep("r_", all_names, value=TRUE))]
                       tmp_la <- tmp[,c(geo_id_names,grep("r_", all_names, value=TRUE))]
                       tmp_l[,grep("r_", all_names, value=TRUE)] <- proportions(as.matrix(tmp[,grep("r_", all_names, value=TRUE)]), 2)
                       tmp_l <- tmp_l[order(tmp_l[,1]),]
                       tmp[,grep("r_", all_names, value=TRUE)] <- tmp_l[,-1]
                       return(list(n_rc = t(n_rc),
                                   probs = tmp[,-ncol(tmp)],
                                   tots = tmp_la))
                     })
  n_rc <- do.call(rbind, lapply(tmp_tabs, function(x)x$n_rc))
  
  r_g_t <- do.call(rbind, lapply(tmp_tabs, function(x)x$probs))
  N_rg <- do.call(rbind, lapply(tmp_tabs, function(x)x$tots))
  theta <- proportions(as.matrix(N_rg[, grep("r_", names(N_rg), value=TRUE)]), 2)
  N_c <- sum(as.matrix(n_rc))#rowSums(as.matrix(n_rc))
  alpha <- prop.table(n_rc, 1)
  lambda <- proportions(colSums(as.matrix(N_rg[, grep("r_", names(N_rg), value=TRUE)])))
  rm(race_pred_args)
  ##Subset to geo's in vf
  r_g_t_geo <- do.call(paste, r_g_t[,geo_id_names])
  N_rg_geo <- do.call(paste, N_rg[,geo_id_names])
  N_rg <- N_rg[N_rg_geo %in% geo_id, ] 
  theta <- theta[N_rg_geo %in% geo_id,] 
  r_g_t <- r_g_t[r_g_t_geo %in% geo_id, ]
  if(nrow(r_g_t) != length(unique(geo_id))){
    stop("Some records in voter.file have unique geographic locations that I wasn't able to find in the census.data.\n
          Records may have mis-matched geographic units that do not exist in the census.")
  }


  ##Split data by geographic cluster
  voter.file$state_cluster <- with(voter.file, paste0(state, voter.file[,geo.cluster]))
  N_rg$state_cluster <- with(N_rg, paste0(state, N_rg[,geo.cluster]))
  theta <- split(as.data.frame(theta), N_rg$state_cluster)
  N_rg <- split(N_rg, N_rg$state_cluster)
  #N_c <- N_c[names(N_c) %in% names(N_rg)]
  alpha <- alpha[rownames(alpha) %in% names(N_rg),]
  #n_rc <- split(n_rc, rownames(n_rc))
  r_g_t$state_cluster <- with(r_g_t, paste0(state, r_g_t[,geo.cluster]))
  r_g_t <- split(r_g_t, r_g_t$state_cluster)
  geo_id <- split(geo_id, voter.file$state_cluster)
  r_g_t <- mapply(function(tab_, tot_, gid_, g_n_){
                    r_g_t_geo_new <- do.call(paste, tab_[,g_n_])
                    Nrg_geo_new <- do.call(paste, tot_[,g_n_])
                    geo_ <- match(gid_, r_g_t_geo_new)
                    Nrg_ord <- match(r_g_t_geo_new, Nrg_geo_new)
                    tot_ <- t(tot_[Nrg_ord,grep("r_", colnames(tot_))]) ## Races in rows
                    tab_ <- t(tab_[,grep("r_", colnames(tab_))]) ## ditto
                    return(list(geo_ = geo_,
                           tab_ = tab_,
                           tot_ = tot_))
                  }, r_g_t, N_rg, geo_id,
                  MoreArgs = list(g_n_ = geo_id_names),
                  SIMPLIFY = FALSE)
  
  orig_ord <- split(voter.file$rec_id_, voter.file$state_cluster)
  n_groups <- length(orig_ord)
  ## Create name indeces
  name_data <- vector("list", 3)
  names(name_data) <- c("last","first","middle") 
  if(ctrl$verbose){
    cat("Pre-processing names...\n")
  }
  if(ctrl$census.dict){
  last_name_orig <- census.names
  names(last_name_orig) <- names(wruData::last)
  }
  for(ntype in c("last","first","middle")){
    if(ntype %in% name_types){
      ntab <- switch(ntype, 
                     last=as.data.frame(if(ctrl$census.dict){last_name_orig}else{wruData::last}),
                     middle=as.data.frame(wruData::mid),
                     first=as.data.frame(wruData::first))
      str_names <- toupper(ntab[,1])
      proc_names_str <- .name_preproc(voter.file[,ntype], c(str_names))
      u_obs_names <- unique(proc_names_str)
      kw_in_ind <- str_names %in% proc_names_str
      keynames_str <- str_names[kw_in_ind]
      u_kw <- unique(keynames_str)
      n_u_kw <- length(u_kw)
      reord <- order(match(u_obs_names, u_kw))
      u_obs_names <- u_obs_names[reord]
      w_names <- match(proc_names_str, u_obs_names)
      w_names <- split(w_names, voter.file$state_cluster)
      pi_tilde <- as.matrix(ntab[which(kw_in_ind),-1])#proportions(as.matrix(ntab[which(kw_in_ind),-1]), 2) 
      name_data[[ntype]] <- list(record_name_id = w_names,
                                 census_table = t(pi_tilde))
    } else {
      name_data[[ntype]] <- list(record_name_id = replicate(n_groups, vector("integer"), simplify=FALSE),
                                 census_table = matrix(NA, 0,0) )
    }
  }
  
  ## Get average number of records per race
  
  ## ... and split inits by county
  race.init <- split(race.init, voter.file$state_cluster)

  
  ## Name selector
  which_names <- switch(namesToUse,
                        'last' = 0L,
                        'last, first' = 1L,
                        'last, first, middle' = 2L,
                        )
  
  ## Run Gibbs sampler
  if(ctrl$verbose){
    cat("Sampling races...\n")
  }
  race_samples <- lapply(seq.int(n_groups),
                         function(cluster){
                           tmp <- hwru_sample(name_data[["last"]]$record_name_id[[cluster]] - 1L,
                                              name_data[["first"]]$record_name_id[[cluster]] - 1L,
                                              name_data[["middle"]]$record_name_id[[cluster]] - 1L,
                                              r_g_t[[cluster]]$geo_ - 1L,
                                              r_g_t[[cluster]]$tot_,
                                              N_c*100,#[cluster],
                                              alpha[cluster,],
                                              name_data[["last"]]$census_table,
                                              name_data[["first"]]$census_table,
                                              name_data[["middle"]]$census_table,
                                              t(theta[[cluster]]),
                                              lambda,
                                              which_names,
                                              ctrl$iter,
                                              ctrl$burnin,
                                              ctrl$me.correct,
                                              race.init[[cluster]] - 1L,
                                              0)
                           return(cbind(orig_ord[[cluster]], tmp))
                         })
  if(ctrl$verbose){
    cat("Post-processing results and wrapping up.\n")
  }
  ## Get posterior race probabilities and append to voter.file
  race_samples <- do.call(rbind, race_samples)
  race_samples <- race_samples[order(race_samples[,1]), -1]
  race_probs <- proportions(race_samples, 1)
  colnames(race_probs) <- paste0("pred_",race.suff)
  voter.file <- cbind(voter.file[,orig.names], race_probs)
  voter.file$state <- orig.state
  
  ## Return expanded voter.file.
  return(voter.file)
  
}