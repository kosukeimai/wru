#' Race prediction function, for measurement error WRU model
#' 
#' Sample individual-level race/ethnicity from a measurement-error Bayesian Dirichlet-multinomial model.
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
#' @param race.init Initial race for each observation in voter.file. Must be an integer, with
#'                  0=white, 1=black, 2=hispanic, 3=asian, and 4=other. Defaults to values obtained from \code{race_predict}.
#' @param ... Other arguments passed to \code{\link{predict_race}}.
#' @param control List of control arguments, including 
#' \itemize{
#'  \item{iter}{ Number of MCMC iterations. Defaults to 1000.}
#'  \item{burnin}{ Number of iterations discarded as burnin. Defaults to half of \code{iter}.}
#'  \item{table.surnames}{ Data frame. User-provided table of counts by race and last name.} 
#'  \item{table.first}{ Data frame. User-provided table of counts by race and first name.} 
#'  \item{table.middle}{ Data frame. User-provided table of counts by race and middle name.} 
#'  \item{verbose}{ Print progress information. Defaults to \code{TRUE}.}
#'  \item{par.group}{ Required string. Higher level of aggregation for parallel sampling. One of "county", "zipcode", or "tract". 
#'                    Must be a higher level of aggregation than \code{census.geo}. Defaults to "county". See Details for more information.}
#'  \item{me.correct}{ String. Should the model correcting measurement error for \code{names}, \code{races}, \code{both}, \code{neither} (default)?}
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
predict_race_me <- function(voter.file,
                            namesToUse,
                            census.geo,
                            race.init = NULL,
                            ...,
                            control = NULL)
{
  ## Form control list
  ctrl <- list(iter = 1000,
               thin = 1,
               verbose = TRUE, 
               parallel = TRUE,
               par.group = "county",
               me.correct = "neither",
               me.name = FALSE,
               me.race = FALSE,
               race.marginal = c(.6665, .0853, .1367, .0797, .0318),
               table.surnames = NULL,
               table.first = NULL,
               table.middle = NULL,
               seed = sample(1:1000, 1))
  ctrl$burnin <- floor(ctrl$iter/2)
  ctrl[names(control)] <- control
  if(ctrl$me.correct %in% c("names","both")){
    ctrl$me.name <- TRUE
  }
  if(ctrl$me.correct %in% c("races","both")){
    ctrl$me.race <- TRUE
  }
  
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
  stopifnot(ctrl$par.group %in% c("county","tract","zipcode"),
            all(!is.na(voter.file$last)))
  if(!(ctrl$par.group %in% colnames(voter.file))){
    stop(sprintf("For the parallel estimation, I need information on each record's %s", ctrl$par.group))
  }
  if(!(ctrl$me.correct %in% c("names","races","both","neither"))){
    stop(sprintf("me.correct must be one of 'names','races','both', or 'neither'"))
  }
  
  orig.names <- names(voter.file)
  orig.state <- voter.file$state
  voter.file$state <- toupper(voter.file$state)
  voter.file$rec_id_ <- 1:nrow(voter.file)
  
  
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
                       tmp_la <- tmp[,c(geo_id_names,grep("r_", all_names, value=TRUE))]
                       return(list(tots = tmp_la))
                     })
  N_rg <- do.call(rbind, lapply(tmp_tabs, function(x)x$tots))
  zero_ind <- rowSums(N_rg[, grep("r_", names(N_rg), value=TRUE)]) < 1
  n_zero <- sum(zero_ind)
  N_rg[zero_ind, grep("r_", names(N_rg), value=TRUE)] <- t(rmultinom(n_zero, 1, ctrl$race.marginal))
  rm(race_pred_args)
  ##Subset to geo's in vf
  N_rg_geo <- do.call(paste, N_rg[,geo_id_names])
  N_rg <- N_rg[N_rg_geo %in% geo_id, ] 
  if(ctrl$me.race){
    alpha_ <- t(apply(as.matrix(N_rg[, grep("r_", names(N_rg), value=TRUE)]), 1,
                    function(x){
                      oprop <- x/sum(x)
                      zero_ind <- x==0
                      x[zero_ind] <- 0.5/(sum(x) + 5/2)
                      zeta_n <- sum(x[zero_ind])
                      x[!zero_ind] <- oprop[!zero_ind] * (1.0 - zeta_n)
                      return(x)
                    }))
  } else {
    alpha_ <- proportions(as.matrix(N_rg[, grep("r_", names(N_rg), value=TRUE)]), 1)
  }
  if(nrow(N_rg) != length(unique(geo_id))){
    stop("Some records in voter.file have geographic locations that I wasn't able to find in the census.data.\n
          Records may have mis-matched geographic units that do not exist in the census.")
  }
  
  
  ##Split data by geographic cluster
  voter.file$state_cluster <- with(voter.file, paste0(state, voter.file[,ctrl$par.group]))
  N_rg$state_cluster <- with(N_rg, paste0(state, N_rg[,ctrl$par.group]))
  alpha_ <- split(as.data.frame(alpha_), N_rg$state_cluster)
  N_rg <- split(N_rg, N_rg$state_cluster)
  geo_id <- split(geo_id, voter.file$state_cluster)
  r_g_t <- mapply(function(tab_, tot_, gid_, g_n_){
    Nrg_geo_new <- do.call(paste, tot_[,g_n_])
    geo_ <- match(gid_, Nrg_geo_new)
    tot_ <- t(tot_[,grep("r_", colnames(tot_))]) ## Races in rows
    tab_ <- t(tab_[,grep("r_", colnames(tab_))]) ## ditto
    return(list(geo_ = geo_,
                alpha_ = tab_,
                N_rg_ = tot_))
  }, alpha_, N_rg, geo_id,
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
  if(!is.null(ctrl$table.surnames)){
    last_name_orig <- ctrl$table.surnames
    names(last_name_orig) <- names(wruData::last_c)
  }
  if(!is.null(ctrl$table.first)){
    first_name_orig <- ctrl$table.first
    names(first_name_orig) <- names(wruData::first_c)
  }
  if(!is.null(ctrl$table.middle)){
    middle_name_orig <- ctrl$table.middle
    names(middle_name_orig) <- names(wruData::mid_c)
  }
  for(ntype in c("last","first","middle")){
    if(ntype %in% name_types){
      ntab <- switch(ntype, 
                     last=as.data.frame(if(!is.null(ctrl$table.surnames)){last_name_orig}else{wruData::last_c}),
                     first=as.data.frame(if(!is.null(ctrl$table.first)){first_name_orig}else{wruData::first_c}),
                     middle=as.data.frame(if(!is.null(ctrl$table.middle)){middle_name_orig}else{wruData::mid_c}))
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
      M_ <- as.matrix(ntab[which(kw_in_ind),-1]) + 0.5
      if(ctrl$me.name){
        pi_ <- apply(M_, 2, 
                       function(x){
                         oprop <- x/sum(x)
                         zero_ind <- x==0
                         x[zero_ind] <- 0.5/(sum(x) + 5/2)
                         zeta_n <- sum(x[zero_ind])
                         x[!zero_ind] <- oprop[!zero_ind] * (1.0 - zeta_n)
                         return(x)
                       }
        )
      } else {
        pi_ <- proportions(M_,2)
      }
      name_data[[ntype]] <- list(record_name_id = w_names,
                                 M_ = t(M_),
                                 pi_ = t(pi_))
    } else {
      name_data[[ntype]] <- list(record_name_id = replicate(n_groups, vector("integer"), simplify=FALSE),
                                 M_ = matrix(NA, 0,0),
                                 pi_ = matrix(NA, 0,0) )
    }
  }

  ## Split inits by cluster
  race.init <- split(race.init, voter.file$state_cluster)
  
  
  ## Name selector
  which.names <- switch(namesToUse,
                        'last' = 0L,
                        'last, first' = 1L,
                        'last, first, middle' = 2L,
  )
  
  ## Run Gibbs sampler
  if(ctrl$verbose){
    cat("Sampling races...\n")
    pb <- txtProgressBar(min = 0, max = n_groups, style = 3)
  }
  race_samples <- lapply(seq.int(n_groups),
                         function(cluster){
                           tmp <- sample_me(name_data[["last"]]$record_name_id[[cluster]] - 1L,
                                            name_data[["first"]]$record_name_id[[cluster]] - 1L,
                                            name_data[["middle"]]$record_name_id[[cluster]] - 1L,
                                            r_g_t[[cluster]]$geo_ - 1L,
                                            r_g_t[[cluster]]$N_rg_,
                                            name_data[["last"]]$M_,
                                            name_data[["first"]]$M_,
                                            name_data[["middle"]]$M_,
                                            r_g_t[[cluster]]$alpha_,
                                            name_data[["last"]]$pi_,
                                            name_data[["first"]]$pi_,
                                            name_data[["middle"]]$pi_,
                                            ctrl$race.marginal, 
                                            which.names,
                                            ctrl$iter,
                                            ctrl$burnin,
                                            ctrl$me.name,
                                            ctrl$me.race,
                                            race.init[[cluster]] - 1L,
                                            0)
                           if(ctrl$verbose){
                             setTxtProgressBar(pb, cluster)
                           }
                           return(cbind(orig_ord[[cluster]], tmp))
                         })
  if(ctrl$verbose){
    close(pb)
  }
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