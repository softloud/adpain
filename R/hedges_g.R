#' Hedge's g
#'
#' @param mean_1 Mean first group
#' @param mean_2 Mean second group
#' @param sd_1 standard deviation
#' @param sd_2 standard deviation
#' @param n_1 sample size
#' @param n_2 sample size
#'
#' @return
#' @export
#'
#' @examples

hedges_g <- function(
  mean_1,
  mean_2,
  sd_1,
  sd_2,
  n_1,
  n_2
) {
  sd_pooled <- sqrt(
    (
      (n_1 - 1) * sd_1^2 + (n_2 - 1) * sd_2^2
      ) / (n_1 + n_2 - 2)
  )

  alpha <- n_1 + n_2 - 2

  # j
  gamma(alpha/2) / (sqrt(alpha/2) * gamma((alpha - 1)/2)) *
    # g
    (mean_1 - mean_2) / sd_poooled

}
