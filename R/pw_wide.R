#' Wrangle pairwise
#'
#' @param dat A filtered dataset
#'
#' @return

pw_wide <- function(pw_dat) {
  dat <-
    pw_dat %>%
    mutate(across(c("study", "intervention"), as.character)) %>% 
    select(study, intervention, covidence, mean, sd, r, n)

  # set group2 to placebo if placebo
  # else set comparison to be arbitrary
  # label data with which group is group1 and group2
    
}