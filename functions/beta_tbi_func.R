# title: beta_tbi_func
# description: function to calculate beta diversity

beta_tbi_func <- function(grp, it, occ_df_pa, occ_df_unp) {
  
  # select useful data
  df_pa <- dplyr::select(occ_df_pa, dplyr::starts_with("year"), iteration, species, grp)
  
  df_unp <- dplyr::select(occ_df_unp, dplyr::starts_with("year"), iteration, species, grp)
  
  # prepare data as wide species dataframe for first and last year
  bprep_pa <- df_pa %>%
    dplyr::filter(grp == !!grp, iteration == it) %>%
    # only non-na columns
    dplyr::select(which(colSums(is.na(.)) < 1)) %>%
    # select first and last year
    dplyr::select(1, (ncol(.) - 4), species) %>%
    # make truly long
    tidyr::gather(year, occ, dplyr::starts_with("year")) %>%
    # spread to wide
    tidyr::spread(species, occ) %>%
    dplyr::select(-year)
  
  bprep_unp <- df_unp %>%
    dplyr::filter(grp == !!grp, iteration == it) %>%
    # only non-na columns
    dplyr::select(which(colSums(is.na(.)) < 1)) %>%
    # select first and last year
    dplyr::select(1, (ncol(.) - 4), species) %>%
    # make truly long
    tidyr::gather(year, occ, dplyr::starts_with("year")) %>%
    # spread to wide
    tidyr::spread(species, occ) %>%
    dplyr::select(-year)
  
  # time 1
  t1 <- bind_rows(bprep_pa[1,], bprep_unp[1,])
  
  # time 2
  t2 <- bind_rows(bprep_pa[2,], bprep_unp[2,])
  
  # calculate tbi
  tbi <- adespatial::TBI(t1, t2)
  
  # compile data in easy format
  tbi_df <- data.frame(tbi_val = tbi$TBI, loss = tbi$BCD.mat$`B/(2A+B+C)`, gain = tbi$BCD.mat$`C/(2A+B+C)`, dir = tbi$BCD.mat$Change, prot = c("Protected", "Unprotected"), iteration = it, grp = grp)
  
  return(tbi_df)
  
}