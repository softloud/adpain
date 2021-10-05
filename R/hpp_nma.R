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


    m_dat <-
      dat %>%
      viable_observations()

    m_type <-
      dat %>%
      pull(model_type) %>%
      unique()


    hpp_net(m_dat, m_type) %>%
      safe_nma(trt_effects = "random")
}
