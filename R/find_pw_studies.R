#' Find studies for pairwise comparison
#' 
#' Filters dataset from nma (with study and intervention relabelled) to 
#' given pairwise intervention comparisons.
#'
#' @param dat Cleaned nma dat with study and intervention renamed
#' @param g1 First comparitor for meta-analysis
#' @param g2 Second comparitor for meta-analysis
#'
#' @return
#' @export
#'
#' @examples

find_pw_studies <- function(g1, g2, dat) {
  dat %>% 
    filter(
      intervention %in% c(g1, g2)
    ) %>% 
    select(study, intervention) %>% 
    distinct() %>% 
    pivot_wider(
      names_from = intervention,
      values_from = intervention
    ) %>% 
    drop_na() %>%
    pull(study)
  
}