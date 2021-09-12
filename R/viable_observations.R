#' Function to extract viable arms
#' 
#' Takes set of observations and returns only those with at least two observations
#' per study.
#' 
#' @param hpp_df A subset of `w_obs`.
#' 
#' @export

viable_observations <- function(hpp_df) {
  hpp_df %>% 
    group_by(study) %>% 
    mutate(
      study_obs_count = length(study)
    ) %>%
    select(study_obs_count, everything()) %>% 
    arrange(study_obs_count, study) %>% 
    filter(study_obs_count > 1)  %>% 
    ungroup() 
}