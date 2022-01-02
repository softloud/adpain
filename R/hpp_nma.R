#' Get NMA dataset
#'
#'
#' @param dat Currently mod_dat target
#' @param ... Filters
#'
#' @return
#' @export
#'
#' @examples
#'

get_nma_dat <- function(dat, ...) {
  dat %>%
    filter(...) %>%
    # Error: Single-arm studies are not supported: issue with study
    # Need to ensure there are at least 2 observations (rows) per study
    viable_observations() %>%
    ungroup()

}

#' NMA function for analysis
#'
#' @param dat A datset produced by a group target
#'
#' @return nma
#' @export
#'
#' @examples

hpp_nma <- function(dat) {

  # check the table
  assertthat::assert_that(
    nrow(dat) > 0,
    msg = "Data not viable"
  )

  # if (class(dat) == "character") stop("Data not viable.")

  options(mc.cores = parallel::detectCores() - 1)


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
