#' Using study identifier and title_cov to create a study id
#' 
#' Fiddly and I use it more than once.

study_id <- function(df) {
  df %>%
    select(study_cov, title_cov) %>% 
    distinct() %>% 
    group_by(study_cov) %>% 
    mutate(
      tag = 1:n(),
      max_tag = max(tag),
      study = if_else(
        max_tag > 1,
        glue("{study_cov} : {tag}"),
        study_cov
      )
    ) %>% 
    select(-tag, -max_tag) %>% 
    right_join(df, by = c("study_cov", "title_cov")) %>% 
    ungroup() 
}

