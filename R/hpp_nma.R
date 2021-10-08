#' NMA function for analysis
#'
#' @param dat A datset produced by a group target
#'
#' @return nma
#' @export
#'
#' @examples

hpp_nma <- function(dat) {
  if (class(dat) == "character") stop("Data not viable.")


    m_dat <-
      dat %>%
      viable_observations()

    m_type <-
      dat %>%
      pull(model_type) %>%
      unique()


    hpp_net(m_dat, m_type) %>%
      nma(trt_effects = "random")
}
