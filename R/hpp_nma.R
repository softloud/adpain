#' NMA function for analysis
#'
#' @param dat A datset produced by a group target
#' @param all_dat Entire dataset so placebo arms can be identified
#'
#' @return nma
#' @export
#'
#' @examples

hpp_nma <- function(dat, all_dat) {

    placebo_dat <-
      dat %>%
      select(outcome, study, timepoint, condition) %>%
      distinct() %>%
      inner_join(all_dat %>%
                   filter(type == "placebo"),
                 by = c('outcome', "study", "timepoint"))

    dat <-
      dat %>%
      bind_rows(placebo_dat) %>%
      viable_observations() %>%
      distinct()

    m_type <-
      dat %>%
      pull(model_type) %>%
      unique()


    hpp_net(dat, m_type) %>%
      safe_nma(trt_effects = "random")
}
